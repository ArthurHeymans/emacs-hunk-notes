;;; ai-code-review.el --- Local inline code review comments for AI agents -*- lexical-binding: t; -*-

;; Author: ai-code-review contributors
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, vc, convenience

;;; Commentary:

;; Review any unified diff locally with inline comments, then generate or send a
;; structured prompt for an AI coding agent.

;;; Code:

(require 'diff-mode)
(require 'ai-code-review-core)
(require 'ai-code-review-diff)
(require 'ai-code-review-comments)
(require 'ai-code-review-overlays)
(require 'ai-code-review-storage)
(require 'ai-code-review-prompt)
(require 'ai-code-review-agents)

(declare-function ai-code-review-comment-dwim "ai-code-review-comments" (&rest args))
(declare-function ai-code-review-comment-block-dwim "ai-code-review-comments" (&rest args))
(declare-function ai-code-review-remove-comment "ai-code-review-comments" (&rest args))

(defun ai-code-review--make-mode-map ()
  "Return the keymap for `ai-code-review-mode'."
  (let ((map (make-sparse-keymap)))
    ;; `C-c ,' is in the user/minor-mode-friendly punctuation space and avoids
    ;; common diff-mode bindings such as `C-c C-r'.
    (define-key map (kbd "C-c , c") #'ai-code-review-comment-dwim)
    (define-key map (kbd "C-c , C") #'ai-code-review-comment-block-dwim)
    (define-key map (kbd "C-c , x") #'ai-code-review-remove-comment)
    (define-key map (kbd "C-c , n") #'ai-code-review-next-comment)
    (define-key map (kbd "C-c , p") #'ai-code-review-previous-comment)
    (define-key map (kbd "C-c , l") #'ai-code-review-list-comments)
    (define-key map (kbd "C-c , t") #'ai-code-review-toggle-comments)
    (define-key map (kbd "C-c , r") #'ai-code-review-toggle-resolved)
    (define-key map (kbd "C-c , g") #'ai-code-review-show-prompt)
    (define-key map (kbd "C-c , y") #'ai-code-review-copy-prompt)
    (define-key map (kbd "C-c , s") #'ai-code-review-send-prompt-to-agent)
    map))

(defvar ai-code-review-mode-map nil
  "Keymap for `ai-code-review-mode'.")

(setq ai-code-review-mode-map (ai-code-review--make-mode-map))

;;;###autoload
(define-minor-mode ai-code-review-mode
  "Inline AI code review comments for unified diff buffers.

Key bindings use the `C-c ,' prefix:
\{ai-code-review-mode-map}"
  :lighter " AI-Review"
  :keymap ai-code-review-mode-map
  (if ai-code-review-mode
      (progn
        (unless (derived-mode-p 'diff-mode)
          (diff-mode))
        (unless ai-code-review-comments
          (setq ai-code-review-comments nil))
        (ai-code-review-load-review)
        (ai-code-review-render-comments)
        (message "AI code review mode enabled"))
    (ai-code-review-clear-overlays)))

(defun ai-code-review-start (&rest metadata)
  "Enable `ai-code-review-mode' for the current diff buffer with METADATA.
METADATA is a plist accepting :repo-root, :backend, :base-revision,
:target-revision and :diff-id."
  (setq ai-code-review-repo-root (or (plist-get metadata :repo-root)
                                     ai-code-review-repo-root
                                     default-directory)
        ai-code-review-backend (or (plist-get metadata :backend)
                                   ai-code-review-backend)
        ai-code-review-base-revision (or (plist-get metadata :base-revision)
                                         ai-code-review-base-revision)
        ai-code-review-target-revision (or (plist-get metadata :target-revision)
                                           ai-code-review-target-revision)
        ai-code-review-diff-id (or (plist-get metadata :diff-id)
                                   ai-code-review-diff-id))
  (unless (derived-mode-p 'diff-mode)
    (diff-mode))
  (ai-code-review-mode 1)
  (current-buffer))

(defun ai-code-review-open-diff-buffer (name diff-text &rest metadata)
  "Create or reuse diff buffer NAME containing DIFF-TEXT and start review.
METADATA is forwarded to `ai-code-review-start'."
  (let ((buf (get-buffer-create name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert diff-text)
        (goto-char (point-min)))
      (apply #'ai-code-review-start metadata))
    (pop-to-buffer buf)
    buf))

;;;###autoload
(defun ai-code-review-current-diff-buffer ()
  "Enable AI code review mode in the current unified diff buffer."
  (interactive)
  (ai-code-review-start :repo-root (or ai-code-review-repo-root default-directory)
                        :backend (or ai-code-review-backend 'diff)))

(provide 'ai-code-review)
;;; ai-code-review.el ends here
