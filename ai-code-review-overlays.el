;;; ai-code-review-overlays.el --- Inline overlays for ai-code-review -*- lexical-binding: t; -*-

;;; Commentary:

;; Overlay rendering for local review comments in unified diff buffers.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'ai-code-review-core)
(require 'ai-code-review-diff)

(defvar ai-code-review-mode-map)
(declare-function ai-code-review-comment-dwim "ai-code-review-comments" (&rest args))
(declare-function ai-code-review-comment-block-dwim "ai-code-review-comments" (&rest args))
(declare-function ai-code-review-remove-comment "ai-code-review-comments" (&rest args))
(declare-function ai-code-review-toggle-resolved "ai-code-review-comments" (&rest args))

(defface ai-code-review-comment-face
  '((((class color) (min-colors 88))
     :background "#fff2a8" :foreground "#222222" :extend t)
    (t :inherit highlight :extend t))
  "Face for inline review comment bodies."
  :group 'ai-code-review)

(defface ai-code-review-comment-id-face
  '((((class color) (min-colors 88))
     :background "#ffd84d" :foreground "#111111" :weight bold :extend t)
    (t :inherit font-lock-warning-face :weight bold))
  "Face for inline review comment ids."
  :group 'ai-code-review)

(defface ai-code-review-resolved-comment-face
  '((t :inherit shadow :slant italic :extend t))
  "Face for resolved inline review comment bodies."
  :group 'ai-code-review)

(defface ai-code-review-comment-line-face
  '((t :inherit highlight :extend t))
  "Face used to highlight diff lines that have review comments."
  :group 'ai-code-review)

(defface ai-code-review-comment-marker-face
  '((t :inherit ai-code-review-comment-id-face :weight bold))
  "Face used for the inline review comment marker."
  :group 'ai-code-review)

(defcustom ai-code-review-show-line-markers t
  "When non-nil, show a visible marker before diff lines with comments."
  :type 'boolean
  :group 'ai-code-review)

(defcustom ai-code-review-render-style 'inline-text
  "How to render inline review comments.
The value `inline-text' inserts read-only comment blocks into the diff buffer so
comments are visible as actual buffer text.  The value `overlay' uses virtual
overlay text only."
  :type '(choice (const :tag "Inserted read-only text blocks" inline-text)
                 (const :tag "Overlay virtual text" overlay))
  :group 'ai-code-review)

(defvar-local ai-code-review--overlays nil
  "Overlays currently used to render review comments.")

(defvar-local ai-code-review-comments-visible t
  "Non-nil when inline review comment overlays are visible.")

(defun ai-code-review--delete-inline-comment-blocks ()
  "Delete materialized inline review comment blocks from the current buffer."
  (let ((inhibit-read-only t)
        (pos (point-min)))
    (save-excursion
      (while (< pos (point-max))
        (setq pos (or (text-property-any
                       pos (point-max) 'ai-code-review-comment-block t)
                      (point-max)))
        (when (< pos (point-max))
          (let ((end (or (next-single-property-change
                          pos 'ai-code-review-comment-block nil (point-max))
                         (point-max))))
            (if (> end pos)
                (delete-region pos end)
              ;; Defensive progress guard for unusual property boundaries.
              (setq pos (1+ pos)))))))))

(defun ai-code-review-clear-overlays ()
  "Remove all inline review comment render artifacts in the current buffer."
  (mapc #'delete-overlay ai-code-review--overlays)
  (setq ai-code-review--overlays nil)
  (ai-code-review--delete-inline-comment-blocks))

(defun ai-code-review--key-description (command)
  "Return the key description for COMMAND in `ai-code-review-mode-map'."
  (if (and (boundp 'ai-code-review-mode-map)
           (keymapp ai-code-review-mode-map))
      (let ((key (where-is-internal command ai-code-review-mode-map t)))
        (if key (key-description key) "M-x"))
    "M-x"))

(defun ai-code-review--comment-actions-string ()
  "Return a keybinding hint string for inline comment blocks."
  (format "comment: %s  block: %s  remove: %s  resolve: %s"
          (ai-code-review--key-description #'ai-code-review-comment-dwim)
          (ai-code-review--key-description #'ai-code-review-comment-block-dwim)
          (ai-code-review--key-description #'ai-code-review-remove-comment)
          (ai-code-review--key-description #'ai-code-review-toggle-resolved)))

(defun ai-code-review--comment-after-string (comment)
  "Return virtual text used to display COMMENT."
  (let* ((resolved (ai-code-review-comment-resolved comment))
         (body-face (if resolved
                        'ai-code-review-resolved-comment-face
                      'ai-code-review-comment-face))
         (status (if resolved " resolved" ""))
         (prefix (propertize
                  (format "   [%s%s] "
                          (ai-code-review-comment-id comment)
                          status)
                  'face 'ai-code-review-comment-id-face))
         (hint (propertize (format "(%s) "
                                   (ai-code-review--comment-actions-string))
                           'face 'shadow))
         (body (propertize
                (replace-regexp-in-string
                 "\n" "\n          "
                 (or (ai-code-review-comment-body comment) ""))
                'face body-face)))
    (concat "\n" prefix hint body)))

(defun ai-code-review--comment-block-string (comment)
  "Return materialized text block for COMMENT."
  (let* ((id (ai-code-review-comment-id comment))
         (status (if (ai-code-review-comment-resolved comment) " resolved" ""))
         (body (or (ai-code-review-comment-body comment) ""))
         (body-lines (split-string body "\n")))
    (concat
     (format "\n┌─ AI-REVIEW %s%s  %s\n"
             id status (ai-code-review--comment-actions-string))
     (mapconcat (lambda (line) (concat "│ " line)) body-lines "\n")
     (format "\n└─ END AI-REVIEW %s\n" id))))

(defun ai-code-review--insert-comment-block (comment)
  "Insert a read-only inline text block for COMMENT at point."
  (let ((start (point))
        (inhibit-read-only t))
    (insert (ai-code-review--comment-block-string comment))
    (add-text-properties
     start (point)
     `(ai-code-review-comment-block t
       ai-code-review-comment ,comment
       read-only t
       rear-nonsticky t
       face ai-code-review-comment-face
       font-lock-face ai-code-review-comment-face
       help-echo ,(format "AI review comment: %s"
                           (ai-code-review--comment-actions-string))))))

(defun ai-code-review--render-comment-overlay (comment pos)
  "Render COMMENT virtually at POS using overlays."
  (save-excursion
    (goto-char pos)
    (let* ((bol (line-beginning-position))
           (eol (line-end-position))
           (line-ov (make-overlay bol eol nil t nil))
           (marker-ov (make-overlay bol bol nil t nil))
           (body-ov (make-overlay eol eol nil t nil)))
      (dolist (ov (list line-ov marker-ov body-ov))
        (overlay-put ov 'ai-code-review-comment comment)
        (overlay-put ov 'evaporate t)
        (overlay-put ov 'priority 1001))
      (overlay-put line-ov 'face 'ai-code-review-comment-line-face)
      (overlay-put marker-ov 'before-string
                   (when ai-code-review-show-line-markers
                     (propertize
                      (format "💬%s " (ai-code-review-comment-id comment))
                      'face 'ai-code-review-comment-marker-face)))
      (overlay-put body-ov 'after-string
                   (ai-code-review--comment-after-string comment))
      (push line-ov ai-code-review--overlays)
      (push marker-ov ai-code-review--overlays)
      (push body-ov ai-code-review--overlays))))

(defun ai-code-review--render-comment-inline-text (comment pos)
  "Render COMMENT as actual read-only text after POS."
  (save-excursion
    (goto-char pos)
    (let* ((bol (line-beginning-position))
           (eol (line-end-position))
           (line-ov (make-overlay bol eol nil t nil)))
      (overlay-put line-ov 'ai-code-review-comment comment)
      (overlay-put line-ov 'evaporate t)
      (overlay-put line-ov 'priority 1001)
      (overlay-put line-ov 'face 'ai-code-review-comment-line-face)
      (push line-ov ai-code-review--overlays)
      (goto-char eol)
      (ai-code-review--insert-comment-block comment))))

(defun ai-code-review-render-comments ()
  "Render all comments for the current review buffer."
  (interactive)
  (ai-code-review-clear-overlays)
  (when ai-code-review-comments-visible
    (dolist (comment ai-code-review-comments)
      (let ((pos (ai-code-review-diff-find-position
                  (ai-code-review-comment-file comment)
                  (ai-code-review-comment-side comment)
                  (ai-code-review-comment-line-start comment)
                  (ai-code-review-comment-change-type comment))))
        (when pos
          (pcase ai-code-review-render-style
            ('inline-text (ai-code-review--render-comment-inline-text comment pos))
            (_ (ai-code-review--render-comment-overlay comment pos)))))))
  (setq ai-code-review--overlays (nreverse ai-code-review--overlays)))

(defun ai-code-review-hide-comments ()
  "Hide inline review comment overlays."
  (interactive)
  (setq ai-code-review-comments-visible nil)
  (ai-code-review-clear-overlays)
  (force-mode-line-update)
  (message "AI code review comments hidden"))

(defun ai-code-review-show-inline-comments ()
  "Show inline review comment overlays."
  (interactive)
  (setq ai-code-review-comments-visible t)
  (ai-code-review-render-comments)
  (force-mode-line-update)
  (message "AI code review comments shown"))

(defun ai-code-review-toggle-comments ()
  "Toggle inline review comment overlay visibility."
  (interactive)
  (if ai-code-review-comments-visible
      (ai-code-review-hide-comments)
    (ai-code-review-show-inline-comments)))

(defalias 'ai-code-review-show-comments #'ai-code-review-show-inline-comments)

(provide 'ai-code-review-overlays)
;;; ai-code-review-overlays.el ends here
