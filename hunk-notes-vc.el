;;; hunk-notes-vc.el --- VC entry points for hunk-notes -*- lexical-binding: t; -*-

;;; Commentary:

;; Generic `vc' entry points where possible, with Git fallback for diff creation.

;;; Code:

(require 'vc)
(require 'hunk-notes)
(require 'hunk-notes-git)

(defun hunk-notes-vc-root ()
  "Return a VC root for the current buffer or directory."
  (or (vc-root-dir)
      (user-error "Not in a VC-controlled tree")))

(defun hunk-notes-vc-backend ()
  "Return the current VC backend symbol."
  (or (vc-responsible-backend default-directory)
      (user-error "Could not determine VC backend")))

;;;###autoload
(defun hunk-notes-vc-review-revision (revision)
  "Review VC REVISION.
Currently this uses Git for Git-backed repositories."
  (interactive "sVC revision: ")
  (pcase (hunk-notes-vc-backend)
    ('Git (hunk-notes-git-review-revision revision))
    (backend (user-error "hunk-notes does not yet know how to diff VC backend %s" backend))))

;;;###autoload
(defun hunk-notes-vc-review-range (base target)
  "Review VC diff from BASE to TARGET.
Currently this uses Git for Git-backed repositories."
  (interactive "sVC base revision: \nsVC target revision: ")
  (pcase (hunk-notes-vc-backend)
    ('Git (hunk-notes-git-review-range base target))
    (backend (user-error "hunk-notes does not yet know how to diff VC backend %s" backend))))

;;;###autoload
(defun hunk-notes-vc-review-working-copy ()
  "Review the current VC working-copy diff."
  (interactive)
  (pcase (hunk-notes-vc-backend)
    ('Git (hunk-notes-git-review-working-copy))
    (backend (user-error "hunk-notes does not yet know how to diff VC backend %s" backend))))

(provide 'hunk-notes-vc)
;;; hunk-notes-vc.el ends here
