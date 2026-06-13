;;; hunk-notes-core.el --- Core data model for local hunk notes -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Copyright (C) 2026

;; Author: hunk-notes contributors
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, vc, convenience
;; URL: https://example.invalid/hunk-notes

;;; Commentary:

;; Backend-independent review state and comment records.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup hunk-notes nil
  "Local inline review comments for diffs with AI-agent prompt export."
  :group 'tools
  :prefix "hunk-notes-")

(cl-defstruct (hunk-notes-comment
               (:constructor hunk-notes-comment-create))
  id
  repo-root
  backend
  base-revision
  target-revision
  file
  side
  line-start
  line-end
  change-type
  hunk-header
  anchor-context
  body
  resolved
  created-at
  updated-at
  old-line-start
  old-line-end
  new-line-start
  new-line-end)

(defvar-local hunk-notes-comments nil
  "Review comments for the current diff buffer.")

(defvar-local hunk-notes-repo-root nil
  "Repository root associated with the current review buffer.")

(defvar-local hunk-notes-backend nil
  "Version-control backend symbol for the current review buffer.")

(defvar-local hunk-notes-base-revision nil
  "Base revision for the current review buffer.")

(defvar-local hunk-notes-target-revision nil
  "Target revision for the current review buffer.")

(defvar-local hunk-notes-diff-id nil
  "Optional stable identifier for the diff in the current review buffer.")

(defcustom hunk-notes-auto-save t
  "When non-nil, save comments after comment mutations if storage is loaded."
  :type 'boolean
  :group 'hunk-notes)

(defcustom hunk-notes-show-comments-on-enable t
  "When non-nil, show inline comments when review mode is enabled explicitly.
Auto-enabled diff buffers use `hunk-notes-auto-enable-show-comments'
instead."
  :type 'boolean
  :group 'hunk-notes)

(defun hunk-notes--now-string ()
  "Return an ISO-like timestamp for comment metadata."
  (format-time-string "%Y-%m-%dT%H:%M:%S%z"))

(defun hunk-notes-comment-safe-old-line-start (comment)
  "Return COMMENT's old-side start line, tolerating pre-range-slot structs."
  (ignore-errors (hunk-notes-comment-old-line-start comment)))

(defun hunk-notes-comment-safe-old-line-end (comment)
  "Return COMMENT's old-side end line, tolerating pre-range-slot structs."
  (ignore-errors (hunk-notes-comment-old-line-end comment)))

(defun hunk-notes-comment-safe-new-line-start (comment)
  "Return COMMENT's new-side start line, tolerating pre-range-slot structs."
  (ignore-errors (hunk-notes-comment-new-line-start comment)))

(defun hunk-notes-comment-safe-new-line-end (comment)
  "Return COMMENT's new-side end line, tolerating pre-range-slot structs."
  (ignore-errors (hunk-notes-comment-new-line-end comment)))

(defun hunk-notes-comment-normalize (comment)
  "Normalize COMMENT in place after struct layout changes.
During development the old/new range slots briefly lived before
`change-type'.  Live comments created with that layout can still exist in an
Emacs session after reloading the package.  Detect that shifted shape and move
the slots to the current layout, where optional range slots are appended at the
end for better reload compatibility."
  (when (and (>= (length comment) 21)
             ;; In the shifted layout, BODY's current slot contains a line
             ;; number/nil and the real body is four slots later.
             (not (stringp (aref comment 13)))
             (stringp (aref comment 17)))
    (let ((old-line-start (aref comment 10))
          (old-line-end (aref comment 11))
          (new-line-start (aref comment 12))
          (new-line-end (aref comment 13))
          (change-type (aref comment 14))
          (hunk-header (aref comment 15))
          (anchor-context (aref comment 16))
          (body (aref comment 17))
          (resolved (aref comment 18))
          (created-at (aref comment 19))
          (updated-at (aref comment 20)))
      (aset comment 10 change-type)
      (aset comment 11 hunk-header)
      (aset comment 12 anchor-context)
      (aset comment 13 body)
      (aset comment 14 resolved)
      (aset comment 15 created-at)
      (aset comment 16 updated-at)
      (aset comment 17 old-line-start)
      (aset comment 18 old-line-end)
      (aset comment 19 new-line-start)
      (aset comment 20 new-line-end)))
  comment)

(defun hunk-notes-normalize-comments (&optional comments)
  "Normalize COMMENTS, defaulting to current buffer comments."
  (dolist (comment (or comments hunk-notes-comments))
    (hunk-notes-comment-normalize comment)))

(defun hunk-notes--buffer-diff-string ()
  "Return the current buffer's diff text without review comment blocks.
Inline materialized comment blocks are buffer text for visibility, but they are
not part of the underlying diff and should not be exported as diff content."
  (let ((text nil))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (unless (get-text-property (line-beginning-position)
                                   'hunk-notes-comment-block)
          (push (buffer-substring-no-properties
                 (line-beginning-position)
                 (min (point-max) (1+ (line-end-position))))
                text))
        (forward-line 1)))
    (apply #'concat (nreverse text))))

(defun hunk-notes-current-review-metadata ()
  "Return metadata for the current review buffer as an alist."
  `((repo-root . ,(or hunk-notes-repo-root default-directory))
    (backend . ,hunk-notes-backend)
    (base-revision . ,hunk-notes-base-revision)
    (target-revision . ,hunk-notes-target-revision)
    (diff-id . ,hunk-notes-diff-id)
    (buffer-name . ,(buffer-name))))

(defun hunk-notes-review-key ()
  "Return a stable-ish key identifying the current review buffer."
  (let ((repo (file-truename (or hunk-notes-repo-root default-directory)))
        (backend hunk-notes-backend)
        (base hunk-notes-base-revision)
        (target hunk-notes-target-revision)
        (diff-id (or hunk-notes-diff-id (buffer-name))))
    (secure-hash 'sha1 (prin1-to-string (list repo backend base target diff-id)))))

(defun hunk-notes-comment-to-plist (comment)
  "Convert COMMENT to a property list suitable for serialization."
  (hunk-notes-comment-normalize comment)
  (list :id (hunk-notes-comment-id comment)
        :repo-root (hunk-notes-comment-repo-root comment)
        :backend (hunk-notes-comment-backend comment)
        :base-revision (hunk-notes-comment-base-revision comment)
        :target-revision (hunk-notes-comment-target-revision comment)
        :file (hunk-notes-comment-file comment)
        :side (hunk-notes-comment-side comment)
        :line-start (hunk-notes-comment-line-start comment)
        :line-end (hunk-notes-comment-line-end comment)
        :old-line-start (hunk-notes-comment-safe-old-line-start comment)
        :old-line-end (hunk-notes-comment-safe-old-line-end comment)
        :new-line-start (hunk-notes-comment-safe-new-line-start comment)
        :new-line-end (hunk-notes-comment-safe-new-line-end comment)
        :change-type (hunk-notes-comment-change-type comment)
        :hunk-header (hunk-notes-comment-hunk-header comment)
        :anchor-context (hunk-notes-comment-anchor-context comment)
        :body (hunk-notes-comment-body comment)
        :resolved (hunk-notes-comment-resolved comment)
        :created-at (hunk-notes-comment-created-at comment)
        :updated-at (hunk-notes-comment-updated-at comment)))

(defun hunk-notes-comment-from-plist (plist)
  "Create an `hunk-notes-comment' from PLIST."
  (hunk-notes-comment-create
   :id (plist-get plist :id)
   :repo-root (plist-get plist :repo-root)
   :backend (plist-get plist :backend)
   :base-revision (plist-get plist :base-revision)
   :target-revision (plist-get plist :target-revision)
   :file (plist-get plist :file)
   :side (plist-get plist :side)
   :line-start (plist-get plist :line-start)
   :line-end (plist-get plist :line-end)
   :old-line-start (plist-get plist :old-line-start)
   :old-line-end (plist-get plist :old-line-end)
   :new-line-start (plist-get plist :new-line-start)
   :new-line-end (plist-get plist :new-line-end)
   :change-type (plist-get plist :change-type)
   :hunk-header (plist-get plist :hunk-header)
   :anchor-context (plist-get plist :anchor-context)
   :body (plist-get plist :body)
   :resolved (plist-get plist :resolved)
   :created-at (plist-get plist :created-at)
   :updated-at (plist-get plist :updated-at)))

(defun hunk-notes--maybe-refresh (&optional persist)
  "Refresh overlays and optionally persist comments.
PERSIST requests storage only when storage support is available."
  (hunk-notes-normalize-comments)
  (when (fboundp 'hunk-notes-render-comments)
    (hunk-notes-render-comments))
  (force-mode-line-update)
  (when (and persist hunk-notes-auto-save
             (fboundp 'hunk-notes-save-review))
    (ignore-errors (hunk-notes-save-review))))

(provide 'hunk-notes-core)
;;; hunk-notes-core.el ends here
