;;; hunk-notes-overlays.el --- Inline overlays for hunk-notes -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Overlay rendering for local review comments in unified diff buffers.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'hunk-notes-core)
(require 'hunk-notes-diff)

(defvar hunk-notes-mode-map)
(declare-function hunk-notes-comment-dwim "hunk-notes-comments" (&rest args))
(declare-function hunk-notes-comment-block-dwim "hunk-notes-comments" (&rest args))
(declare-function hunk-notes-remove-comment "hunk-notes-comments" (&rest args))
(declare-function hunk-notes-toggle-resolved "hunk-notes-comments" (&rest args))

(defface hunk-notes-comment-face
  '((((class color) (min-colors 88))
     :background "#fff2a8" :foreground "#222222" :extend t)
    (t :inherit highlight :extend t))
  "Face for inline review comment bodies."
  :group 'hunk-notes)

(defface hunk-notes-comment-id-face
  '((((class color) (min-colors 88))
     :background "#ffd84d" :foreground "#111111" :weight bold :extend t)
    (t :inherit font-lock-warning-face :weight bold))
  "Face for inline review comment ids."
  :group 'hunk-notes)

(defface hunk-notes-resolved-comment-face
  '((t :inherit shadow :slant italic :extend t))
  "Face for resolved inline review comment bodies."
  :group 'hunk-notes)

(defface hunk-notes-comment-line-face
  '((t :inherit highlight :extend t))
  "Face used to highlight diff lines that have review comments."
  :group 'hunk-notes)

(defface hunk-notes-comment-marker-face
  '((t :inherit hunk-notes-comment-id-face :weight bold))
  "Face used for the inline review comment marker."
  :group 'hunk-notes)

(defcustom hunk-notes-show-line-markers t
  "When non-nil, show a visible marker before diff lines with comments."
  :type 'boolean
  :group 'hunk-notes)

(defcustom hunk-notes-render-style 'inline-text
  "How to render inline review comments.
The value `inline-text' inserts read-only comment blocks into the diff buffer so
comments are visible as actual buffer text.  The value `overlay' uses virtual
overlay text only."
  :type '(choice (const :tag "Inserted read-only text blocks" inline-text)
                 (const :tag "Overlay virtual text" overlay))
  :group 'hunk-notes)

(defvar-local hunk-notes--overlays nil
  "Overlays currently used to render review comments.")

(defvar-local hunk-notes-comments-visible t
  "Non-nil when inline review comment overlays are visible.")

(defun hunk-notes--delete-inline-comment-blocks ()
  "Delete materialized inline review comment blocks from the current buffer."
  (let ((inhibit-read-only t)
        (pos (point-min)))
    (save-excursion
      (while (< pos (point-max))
        (setq pos (or (text-property-any
                       pos (point-max) 'hunk-notes-comment-block t)
                      (point-max)))
        (when (< pos (point-max))
          (let ((end (or (next-single-property-change
                          pos 'hunk-notes-comment-block nil (point-max))
                         (point-max))))
            (if (> end pos)
                (delete-region pos end)
              ;; Defensive progress guard for unusual property boundaries.
              (setq pos (1+ pos)))))))))

(defun hunk-notes-clear-overlays ()
  "Remove all inline review comment render artifacts in the current buffer."
  (mapc #'delete-overlay hunk-notes--overlays)
  (setq hunk-notes--overlays nil)
  (hunk-notes--delete-inline-comment-blocks))

(defun hunk-notes--key-description (command)
  "Return the key description for COMMAND in `hunk-notes-mode-map'."
  (if (and (boundp 'hunk-notes-mode-map)
           (keymapp hunk-notes-mode-map))
      (let ((key (where-is-internal command hunk-notes-mode-map t)))
        (if key (key-description key) "M-x"))
    "M-x"))

(defun hunk-notes--comment-actions-string ()
  "Return a keybinding hint string for inline comment blocks."
  (format "comment: %s  block: %s  remove: %s  resolve: %s"
          (hunk-notes--key-description #'hunk-notes-comment-dwim)
          (hunk-notes--key-description #'hunk-notes-comment-block-dwim)
          (hunk-notes--key-description #'hunk-notes-remove-comment)
          (hunk-notes--key-description #'hunk-notes-toggle-resolved)))

(defun hunk-notes--comment-after-string (comment)
  "Return virtual text used to display COMMENT."
  (let* ((resolved (hunk-notes-comment-resolved comment))
         (body-face (if resolved
                        'hunk-notes-resolved-comment-face
                      'hunk-notes-comment-face))
         (status (if resolved " resolved" ""))
         (prefix (propertize
                  (format "   [%s%s] "
                          (hunk-notes-comment-id comment)
                          status)
                  'face 'hunk-notes-comment-id-face))
         (hint (propertize (format "(%s) "
                                   (hunk-notes--comment-actions-string))
                           'face 'shadow))
         (body (propertize
                (replace-regexp-in-string
                 "\n" "\n          "
                 (or (hunk-notes-comment-body comment) ""))
                'face body-face)))
    (concat "\n" prefix hint body)))

(defun hunk-notes--comment-block-string (comment)
  "Return materialized text block for COMMENT."
  (let* ((id (hunk-notes-comment-id comment))
         (status (if (hunk-notes-comment-resolved comment) " resolved" ""))
         (body (or (hunk-notes-comment-body comment) ""))
         (body-lines (split-string body "\n")))
    (concat
     (format "\n┌─ AI-REVIEW %s%s  %s\n"
             id status (hunk-notes--comment-actions-string))
     (mapconcat (lambda (line) (concat "│ " line)) body-lines "\n")
     (format "\n└─ END AI-REVIEW %s\n" id))))

(defun hunk-notes--insert-comment-block (comment)
  "Insert a read-only inline text block for COMMENT at point."
  (let ((start (point))
        (inhibit-read-only t))
    (insert (hunk-notes--comment-block-string comment))
    (add-text-properties
     start (point)
     `(hunk-notes-comment-block t
       hunk-notes-comment ,comment
       read-only t
       rear-nonsticky t
       face hunk-notes-comment-face
       font-lock-face hunk-notes-comment-face
       help-echo ,(format "Hunk notes comment: %s"
                           (hunk-notes--comment-actions-string))))))

(defun hunk-notes--render-comment-overlay (comment pos)
  "Render COMMENT virtually at POS using overlays."
  (save-excursion
    (goto-char pos)
    (let* ((bol (line-beginning-position))
           (eol (line-end-position))
           (line-ov (make-overlay bol eol nil t nil))
           (marker-ov (make-overlay bol bol nil t nil))
           (body-ov (make-overlay eol eol nil t nil)))
      (dolist (ov (list line-ov marker-ov body-ov))
        (overlay-put ov 'hunk-notes-comment comment)
        (overlay-put ov 'evaporate t)
        (overlay-put ov 'priority 1001))
      (overlay-put line-ov 'face 'hunk-notes-comment-line-face)
      (overlay-put marker-ov 'before-string
                   (when hunk-notes-show-line-markers
                     (propertize
                      (format "💬%s " (hunk-notes-comment-id comment))
                      'face 'hunk-notes-comment-marker-face)))
      (overlay-put body-ov 'after-string
                   (hunk-notes--comment-after-string comment))
      (push line-ov hunk-notes--overlays)
      (push marker-ov hunk-notes--overlays)
      (push body-ov hunk-notes--overlays))))

(defun hunk-notes--render-comment-inline-text (comment pos)
  "Render COMMENT as actual read-only text after POS."
  (save-excursion
    (goto-char pos)
    (let* ((bol (line-beginning-position))
           (eol (line-end-position))
           (line-ov (make-overlay bol eol nil t nil)))
      (overlay-put line-ov 'hunk-notes-comment comment)
      (overlay-put line-ov 'evaporate t)
      (overlay-put line-ov 'priority 1001)
      (overlay-put line-ov 'face 'hunk-notes-comment-line-face)
      (push line-ov hunk-notes--overlays)
      (goto-char eol)
      (hunk-notes--insert-comment-block comment))))

(defun hunk-notes-render-comments ()
  "Render all comments for the current review buffer."
  (interactive)
  (hunk-notes-normalize-comments)
  (hunk-notes-clear-overlays)
  (when hunk-notes-comments-visible
    (dolist (comment hunk-notes-comments)
      (let ((pos (hunk-notes-diff-find-position
                  (hunk-notes-comment-file comment)
                  (hunk-notes-comment-side comment)
                  (hunk-notes-comment-line-start comment)
                  (hunk-notes-comment-change-type comment))))
        (when pos
          (pcase hunk-notes-render-style
            ('inline-text (hunk-notes--render-comment-inline-text comment pos))
            (_ (hunk-notes--render-comment-overlay comment pos)))))))
  (setq hunk-notes--overlays (nreverse hunk-notes--overlays)))

(defun hunk-notes-hide-comments ()
  "Hide inline review comment overlays."
  (interactive)
  (setq hunk-notes-comments-visible nil)
  (hunk-notes-clear-overlays)
  (force-mode-line-update)
  (message "hunk notes comments hidden"))

(defun hunk-notes-show-inline-comments ()
  "Show inline review comment overlays."
  (interactive)
  (setq hunk-notes-comments-visible t)
  (hunk-notes-render-comments)
  (force-mode-line-update)
  (message "hunk notes comments shown"))

(defun hunk-notes-toggle-comments ()
  "Toggle inline review comment overlay visibility."
  (interactive)
  (if hunk-notes-comments-visible
      (hunk-notes-hide-comments)
    (hunk-notes-show-inline-comments)))

(defalias 'hunk-notes-show-comments #'hunk-notes-show-inline-comments)

(provide 'hunk-notes-overlays)
;;; hunk-notes-overlays.el ends here
