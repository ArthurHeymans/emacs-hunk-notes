;;; ai-code-review-core.el --- Core data model for local AI code review -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: ai-code-review contributors
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, vc, convenience
;; URL: https://example.invalid/ai-code-review

;;; Commentary:

;; Backend-independent review state and comment records.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup ai-code-review nil
  "Local inline review comments for diffs with AI-agent prompt export."
  :group 'tools
  :prefix "ai-code-review-")

(cl-defstruct (ai-code-review-comment
               (:constructor ai-code-review-comment-create))
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

(defvar-local ai-code-review-comments nil
  "Review comments for the current diff buffer.")

(defvar-local ai-code-review-repo-root nil
  "Repository root associated with the current review buffer.")

(defvar-local ai-code-review-backend nil
  "Version-control backend symbol for the current review buffer.")

(defvar-local ai-code-review-base-revision nil
  "Base revision for the current review buffer.")

(defvar-local ai-code-review-target-revision nil
  "Target revision for the current review buffer.")

(defvar-local ai-code-review-diff-id nil
  "Optional stable identifier for the diff in the current review buffer.")

(defcustom ai-code-review-auto-save t
  "When non-nil, save comments after comment mutations if storage is loaded."
  :type 'boolean
  :group 'ai-code-review)

(defcustom ai-code-review-show-comments-on-enable t
  "When non-nil, show inline comments when review mode is enabled explicitly.
Auto-enabled diff buffers use `ai-code-review-auto-enable-show-comments'
instead."
  :type 'boolean
  :group 'ai-code-review)

(defun ai-code-review--now-string ()
  "Return an ISO-like timestamp for comment metadata."
  (format-time-string "%Y-%m-%dT%H:%M:%S%z"))

(defun ai-code-review-comment-safe-old-line-start (comment)
  "Return COMMENT's old-side start line, tolerating pre-range-slot structs."
  (ignore-errors (ai-code-review-comment-old-line-start comment)))

(defun ai-code-review-comment-safe-old-line-end (comment)
  "Return COMMENT's old-side end line, tolerating pre-range-slot structs."
  (ignore-errors (ai-code-review-comment-old-line-end comment)))

(defun ai-code-review-comment-safe-new-line-start (comment)
  "Return COMMENT's new-side start line, tolerating pre-range-slot structs."
  (ignore-errors (ai-code-review-comment-new-line-start comment)))

(defun ai-code-review-comment-safe-new-line-end (comment)
  "Return COMMENT's new-side end line, tolerating pre-range-slot structs."
  (ignore-errors (ai-code-review-comment-new-line-end comment)))

(defun ai-code-review-comment-normalize (comment)
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

(defun ai-code-review-normalize-comments (&optional comments)
  "Normalize COMMENTS, defaulting to current buffer comments."
  (dolist (comment (or comments ai-code-review-comments))
    (ai-code-review-comment-normalize comment)))

(defun ai-code-review--buffer-diff-string ()
  "Return the current buffer's diff text without review comment blocks.
Inline materialized comment blocks are buffer text for visibility, but they are
not part of the underlying diff and should not be exported as diff content."
  (let ((text nil))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (unless (get-text-property (line-beginning-position)
                                   'ai-code-review-comment-block)
          (push (buffer-substring-no-properties
                 (line-beginning-position)
                 (min (point-max) (1+ (line-end-position))))
                text))
        (forward-line 1)))
    (apply #'concat (nreverse text))))

(defun ai-code-review-current-review-metadata ()
  "Return metadata for the current review buffer as an alist."
  `((repo-root . ,(or ai-code-review-repo-root default-directory))
    (backend . ,ai-code-review-backend)
    (base-revision . ,ai-code-review-base-revision)
    (target-revision . ,ai-code-review-target-revision)
    (diff-id . ,ai-code-review-diff-id)
    (buffer-name . ,(buffer-name))))

(defun ai-code-review-review-key ()
  "Return a stable-ish key identifying the current review buffer."
  (let ((repo (file-truename (or ai-code-review-repo-root default-directory)))
        (backend ai-code-review-backend)
        (base ai-code-review-base-revision)
        (target ai-code-review-target-revision)
        (diff-id (or ai-code-review-diff-id (buffer-name))))
    (secure-hash 'sha1 (prin1-to-string (list repo backend base target diff-id)))))

(defun ai-code-review-comment-to-plist (comment)
  "Convert COMMENT to a property list suitable for serialization."
  (ai-code-review-comment-normalize comment)
  (list :id (ai-code-review-comment-id comment)
        :repo-root (ai-code-review-comment-repo-root comment)
        :backend (ai-code-review-comment-backend comment)
        :base-revision (ai-code-review-comment-base-revision comment)
        :target-revision (ai-code-review-comment-target-revision comment)
        :file (ai-code-review-comment-file comment)
        :side (ai-code-review-comment-side comment)
        :line-start (ai-code-review-comment-line-start comment)
        :line-end (ai-code-review-comment-line-end comment)
        :old-line-start (ai-code-review-comment-safe-old-line-start comment)
        :old-line-end (ai-code-review-comment-safe-old-line-end comment)
        :new-line-start (ai-code-review-comment-safe-new-line-start comment)
        :new-line-end (ai-code-review-comment-safe-new-line-end comment)
        :change-type (ai-code-review-comment-change-type comment)
        :hunk-header (ai-code-review-comment-hunk-header comment)
        :anchor-context (ai-code-review-comment-anchor-context comment)
        :body (ai-code-review-comment-body comment)
        :resolved (ai-code-review-comment-resolved comment)
        :created-at (ai-code-review-comment-created-at comment)
        :updated-at (ai-code-review-comment-updated-at comment)))

(defun ai-code-review-comment-from-plist (plist)
  "Create an `ai-code-review-comment' from PLIST."
  (ai-code-review-comment-create
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

(defun ai-code-review--maybe-refresh (&optional persist)
  "Refresh overlays and optionally persist comments.
PERSIST requests storage only when storage support is available."
  (ai-code-review-normalize-comments)
  (when (fboundp 'ai-code-review-render-comments)
    (ai-code-review-render-comments))
  (force-mode-line-update)
  (when (and persist ai-code-review-auto-save
             (fboundp 'ai-code-review-save-review))
    (ignore-errors (ai-code-review-save-review))))

(provide 'ai-code-review-core)
;;; ai-code-review-core.el ends here
