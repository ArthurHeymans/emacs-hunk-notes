;;; hunk-notes-magit.el --- Magit integration for hunk-notes -*- lexical-binding: t; -*-

;;; Commentary:

;; Lightweight Magit integration.  Core review functionality does not depend on
;; Magit; this file is a soft adapter.

;;; Code:

(require 'hunk-notes)
(require 'hunk-notes-git)
(require 'hunk-notes-jj)

(require 'magit nil t)
(require 'transient nil t)

(defun hunk-notes-magit--repo-root ()
  "Return Magit repository root, or a best-effort default."
  (cond
   ((fboundp 'magit-toplevel) (magit-toplevel))
   ((fboundp 'magit-git-root) (magit-git-root))
   (t default-directory)))

(defun hunk-notes-magit--revision-at-point ()
  "Return a Magit revision at point when available."
  (cond
   ((fboundp 'magit-commit-at-point) (magit-commit-at-point))
   ((boundp 'magit-buffer-revision) magit-buffer-revision)
   (t nil)))

;;;###autoload
(defun hunk-notes-magit-review-current ()
  "Start hunk notes for the current Magit diff/revision buffer.
If the current buffer is not a diff buffer but a revision is available, open a
Git revision review instead."
  (interactive)
  (cond
   ((derived-mode-p 'diff-mode)
    (hunk-notes-start :repo-root (hunk-notes-magit--repo-root)
                          :backend 'magit
                          :target-revision (hunk-notes-magit--revision-at-point)
                          :diff-id (format "magit:%s" (buffer-name))))
   ((hunk-notes-magit--revision-at-point)
    (hunk-notes-git-review-revision (hunk-notes-magit--revision-at-point)))
   (t
    (user-error "No Magit diff or revision found here"))))

;;;###autoload
(defun hunk-notes-magit-review-commit (revision)
  "Review Magit/Git REVISION."
  (interactive (list (or (hunk-notes-magit--revision-at-point)
                         (read-string "Commit: "))))
  (hunk-notes-git-review-revision revision))

;;;###autoload
(defun hunk-notes-magit-review-range (base target)
  "Review Magit/Git range BASE to TARGET."
  (interactive "sBase revision: \nsTarget revision: ")
  (hunk-notes-git-review-range base target))

(with-eval-after-load 'magit
  (when (boundp 'magit-status-mode-map)
    ;; Conservative binding that avoids depending on Magit's transient layout.
    (define-key magit-status-mode-map (kbd "C-c R") #'hunk-notes-magit-review-current)))

(provide 'hunk-notes-magit)
;;; hunk-notes-magit.el ends here
