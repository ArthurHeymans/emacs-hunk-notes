;;; hunk-notes-git.el --- Git entry points for hunk-notes -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Commands that create Git diffs and enable `hunk-notes-mode'.

;;; Code:

(require 'subr-x)
(require 'hunk-notes)

(defun hunk-notes-git-root ()
  "Return the current Git repository root, or signal an error."
  (or (locate-dominating-file default-directory ".git")
      (user-error "Not in a Git repository")))

(defun hunk-notes-git-working-copy-args ()
  "Return the Git command arguments for a working-copy diff."
  '("diff" "--no-ext-diff" "--"))

(defun hunk-notes-git-revision-args (revision)
  "Return Git command arguments for showing REVISION as a patch."
  (list "show" "--format=fuller" "--stat" "--patch" revision))

(defun hunk-notes-git-range-args (base target)
  "Return Git command arguments for diffing BASE to TARGET."
  (list "diff" "--no-ext-diff" base target "--"))

(defun hunk-notes-git--run (root args)
  "Run Git in ROOT with ARGS and return stdout as a string."
  (unless (executable-find "git")
    (user-error "git executable not found"))
  (let ((default-directory root))
    (with-temp-buffer
      (let ((status (apply #'process-file "git" nil t nil args)))
        (unless (zerop status)
          (user-error "git %s failed:\n%s"
                      (string-join args " ")
                      (buffer-string))))
      (buffer-string))))

(defun hunk-notes-git--review (root args buffer-name base target diff-id)
  "Create a review buffer from Git ARGS in ROOT."
  (let ((diff (hunk-notes-git--run root args)))
    (hunk-notes-open-diff-buffer
     buffer-name diff
     :repo-root root
     :backend 'git
     :base-revision base
     :target-revision target
     :diff-id diff-id)))

;;;###autoload
(defun hunk-notes-git-review-working-copy ()
  "Review `git diff' for the current working copy."
  (interactive)
  (let ((root (hunk-notes-git-root)))
    (hunk-notes-git--review
     root (hunk-notes-git-working-copy-args)
     "*hunk-notes git working copy*" nil "working-copy" "git-working-copy")))

;;;###autoload
(defun hunk-notes-git-review-revision (revision)
  "Review Git REVISION with `git show'."
  (interactive "sGit revision: ")
  (let ((root (hunk-notes-git-root)))
    (hunk-notes-git--review
     root (hunk-notes-git-revision-args revision)
     (format "*hunk-notes git %s*" revision)
     nil revision (format "git-revision:%s" revision))))

;;;###autoload
(defun hunk-notes-git-review-range (base target)
  "Review Git diff from BASE to TARGET."
  (interactive "sGit base revision: \nsGit target revision: ")
  (let ((root (hunk-notes-git-root)))
    (hunk-notes-git--review
     root (hunk-notes-git-range-args base target)
     (format "*hunk-notes git %s..%s*" base target)
     base target (format "git-range:%s..%s" base target))))

(provide 'hunk-notes-git)
;;; hunk-notes-git.el ends here
