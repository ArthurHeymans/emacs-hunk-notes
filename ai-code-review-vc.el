;;; ai-code-review-vc.el --- VC entry points for ai-code-review -*- lexical-binding: t; -*-

;;; Commentary:

;; Generic `vc' entry points where possible, with Git fallback for diff creation.

;;; Code:

(require 'vc)
(require 'ai-code-review)
(require 'ai-code-review-git)

(defun ai-code-review-vc-root ()
  "Return a VC root for the current buffer or directory."
  (or (vc-root-dir)
      (user-error "Not in a VC-controlled tree")))

(defun ai-code-review-vc-backend ()
  "Return the current VC backend symbol."
  (or (vc-responsible-backend default-directory)
      (user-error "Could not determine VC backend")))

;;;###autoload
(defun ai-code-review-vc-review-revision (revision)
  "Review VC REVISION.
Currently this uses Git for Git-backed repositories."
  (interactive "sVC revision: ")
  (pcase (ai-code-review-vc-backend)
    ('Git (ai-code-review-git-review-revision revision))
    (backend (user-error "ai-code-review does not yet know how to diff VC backend %s" backend))))

;;;###autoload
(defun ai-code-review-vc-review-range (base target)
  "Review VC diff from BASE to TARGET.
Currently this uses Git for Git-backed repositories."
  (interactive "sVC base revision: \nsVC target revision: ")
  (pcase (ai-code-review-vc-backend)
    ('Git (ai-code-review-git-review-range base target))
    (backend (user-error "ai-code-review does not yet know how to diff VC backend %s" backend))))

;;;###autoload
(defun ai-code-review-vc-review-working-copy ()
  "Review the current VC working-copy diff."
  (interactive)
  (pcase (ai-code-review-vc-backend)
    ('Git (ai-code-review-git-review-working-copy))
    (backend (user-error "ai-code-review does not yet know how to diff VC backend %s" backend))))

(provide 'ai-code-review-vc)
;;; ai-code-review-vc.el ends here
