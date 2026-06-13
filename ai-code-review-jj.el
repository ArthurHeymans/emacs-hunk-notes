;;; ai-code-review-jj.el --- Jujutsu entry points for ai-code-review -*- lexical-binding: t; -*-

;;; Commentary:

;; First-class Jujutsu (`jj') commands for local AI code review.

;;; Code:

(require 'subr-x)
(require 'ai-code-review)

(defun ai-code-review-jj-root ()
  "Return the current Jujutsu repository root, or signal an error."
  (unless (executable-find "jj")
    (user-error "jj executable not found"))
  (with-temp-buffer
    (let ((status (process-file "jj" nil t nil "root")))
      (unless (zerop status)
        (user-error "Not in a Jujutsu repository")))
    (string-trim (buffer-string))))

(defun ai-code-review-jj-working-copy-args ()
  "Return jj arguments for a working-copy diff."
  '("diff"))

(defun ai-code-review-jj-revision-args (revision)
  "Return jj arguments for diffing REVISION."
  (list "diff" "-r" revision))

(defun ai-code-review-jj-range-args (base target)
  "Return jj arguments for diffing BASE to TARGET."
  (list "diff" "--from" base "--to" target))

(defun ai-code-review-jj--run (root args)
  "Run jj in ROOT with ARGS and return stdout as a string."
  (let ((default-directory root))
    (with-temp-buffer
      (let ((status (apply #'process-file "jj" nil t nil args)))
        (unless (zerop status)
          (user-error "jj %s failed:\n%s"
                      (string-join args " ")
                      (buffer-string))))
      (buffer-string))))

(defun ai-code-review-jj--review (root args buffer-name base target diff-id)
  "Create a review buffer from jj ARGS in ROOT."
  (let ((diff (ai-code-review-jj--run root args)))
    (ai-code-review-open-diff-buffer
     buffer-name diff
     :repo-root root
     :backend 'jj
     :base-revision base
     :target-revision target
     :diff-id diff-id)))

;;;###autoload
(defun ai-code-review-jj-review-working-copy ()
  "Review `jj diff' for the current working copy."
  (interactive)
  (let ((root (ai-code-review-jj-root)))
    (ai-code-review-jj--review
     root (ai-code-review-jj-working-copy-args)
     "*ai-code-review jj working copy*" nil "@" "jj-working-copy")))

;;;###autoload
(defun ai-code-review-jj-review-revision (revision)
  "Review jj REVISION with `jj diff -r REVISION'."
  (interactive "sJujutsu revision: ")
  (let ((root (ai-code-review-jj-root)))
    (ai-code-review-jj--review
     root (ai-code-review-jj-revision-args revision)
     (format "*ai-code-review jj %s*" revision)
     nil revision (format "jj-revision:%s" revision))))

;;;###autoload
(defun ai-code-review-jj-review-range (base target)
  "Review jj diff from BASE to TARGET."
  (interactive "sJujutsu base revision: \nsJujutsu target revision: ")
  (let ((root (ai-code-review-jj-root)))
    (ai-code-review-jj--review
     root (ai-code-review-jj-range-args base target)
     (format "*ai-code-review jj %s..%s*" base target)
     base target (format "jj-range:%s..%s" base target))))

(provide 'ai-code-review-jj)
;;; ai-code-review-jj.el ends here
