;;; ai-code-review-magit.el --- Magit integration for ai-code-review -*- lexical-binding: t; -*-

;;; Commentary:

;; Lightweight Magit integration.  Core review functionality does not depend on
;; Magit; this file is a soft adapter.

;;; Code:

(require 'ai-code-review)
(require 'ai-code-review-git)
(require 'ai-code-review-jj)

(require 'magit nil t)
(require 'transient nil t)

(defun ai-code-review-magit--repo-root ()
  "Return Magit repository root, or a best-effort default."
  (cond
   ((fboundp 'magit-toplevel) (magit-toplevel))
   ((fboundp 'magit-git-root) (magit-git-root))
   (t default-directory)))

(defun ai-code-review-magit--revision-at-point ()
  "Return a Magit revision at point when available."
  (cond
   ((fboundp 'magit-commit-at-point) (magit-commit-at-point))
   ((boundp 'magit-buffer-revision) magit-buffer-revision)
   (t nil)))

;;;###autoload
(defun ai-code-review-magit-review-current ()
  "Start AI code review for the current Magit diff/revision buffer.
If the current buffer is not a diff buffer but a revision is available, open a
Git revision review instead."
  (interactive)
  (cond
   ((derived-mode-p 'diff-mode)
    (ai-code-review-start :repo-root (ai-code-review-magit--repo-root)
                          :backend 'magit
                          :target-revision (ai-code-review-magit--revision-at-point)
                          :diff-id (format "magit:%s" (buffer-name))))
   ((ai-code-review-magit--revision-at-point)
    (ai-code-review-git-review-revision (ai-code-review-magit--revision-at-point)))
   (t
    (user-error "No Magit diff or revision found here"))))

;;;###autoload
(defun ai-code-review-magit-review-commit (revision)
  "Review Magit/Git REVISION."
  (interactive (list (or (ai-code-review-magit--revision-at-point)
                         (read-string "Commit: "))))
  (ai-code-review-git-review-revision revision))

;;;###autoload
(defun ai-code-review-magit-review-range (base target)
  "Review Magit/Git range BASE to TARGET."
  (interactive "sBase revision: \nsTarget revision: ")
  (ai-code-review-git-review-range base target))

(with-eval-after-load 'magit
  (when (boundp 'magit-status-mode-map)
    ;; Conservative binding that avoids depending on Magit's transient layout.
    (define-key magit-status-mode-map (kbd "C-c R") #'ai-code-review-magit-review-current)))

(provide 'ai-code-review-magit)
;;; ai-code-review-magit.el ends here
