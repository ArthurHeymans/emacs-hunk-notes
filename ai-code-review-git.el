;;; ai-code-review-git.el --- Git entry points for ai-code-review -*- lexical-binding: t; -*-

;;; Commentary:

;; Commands that create Git diffs and enable `ai-code-review-mode'.

;;; Code:

(require 'subr-x)
(require 'ai-code-review)

(defun ai-code-review-git-root ()
  "Return the current Git repository root, or signal an error."
  (or (locate-dominating-file default-directory ".git")
      (user-error "Not in a Git repository")))

(defun ai-code-review-git-working-copy-args ()
  "Return the Git command arguments for a working-copy diff."
  '("diff" "--no-ext-diff" "--"))

(defun ai-code-review-git-revision-args (revision)
  "Return Git command arguments for showing REVISION as a patch."
  (list "show" "--format=fuller" "--stat" "--patch" revision))

(defun ai-code-review-git-range-args (base target)
  "Return Git command arguments for diffing BASE to TARGET."
  (list "diff" "--no-ext-diff" base target "--"))

(defun ai-code-review-git--run (root args)
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

(defun ai-code-review-git--review (root args buffer-name base target diff-id)
  "Create a review buffer from Git ARGS in ROOT."
  (let ((diff (ai-code-review-git--run root args)))
    (ai-code-review-open-diff-buffer
     buffer-name diff
     :repo-root root
     :backend 'git
     :base-revision base
     :target-revision target
     :diff-id diff-id)))

;;;###autoload
(defun ai-code-review-git-review-working-copy ()
  "Review `git diff' for the current working copy."
  (interactive)
  (let ((root (ai-code-review-git-root)))
    (ai-code-review-git--review
     root (ai-code-review-git-working-copy-args)
     "*ai-code-review git working copy*" nil "working-copy" "git-working-copy")))

;;;###autoload
(defun ai-code-review-git-review-revision (revision)
  "Review Git REVISION with `git show'."
  (interactive "sGit revision: ")
  (let ((root (ai-code-review-git-root)))
    (ai-code-review-git--review
     root (ai-code-review-git-revision-args revision)
     (format "*ai-code-review git %s*" revision)
     nil revision (format "git-revision:%s" revision))))

;;;###autoload
(defun ai-code-review-git-review-range (base target)
  "Review Git diff from BASE to TARGET."
  (interactive "sGit base revision: \nsGit target revision: ")
  (let ((root (ai-code-review-git-root)))
    (ai-code-review-git--review
     root (ai-code-review-git-range-args base target)
     (format "*ai-code-review git %s..%s*" base target)
     base target (format "git-range:%s..%s" base target))))

(provide 'ai-code-review-git)
;;; ai-code-review-git.el ends here
