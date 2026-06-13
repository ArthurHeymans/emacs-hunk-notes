;;; hunk-notes-jj.el --- Jujutsu entry points for hunk-notes -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; First-class Jujutsu (`jj') commands for local hunk notes.

;;; Code:

(require 'subr-x)
(require 'hunk-notes)

(defun hunk-notes-jj-root ()
  "Return the current Jujutsu repository root, or signal an error."
  (unless (executable-find "jj")
    (user-error "jj executable not found"))
  (with-temp-buffer
    (let ((status (process-file "jj" nil t nil "root")))
      (unless (zerop status)
        (user-error "Not in a Jujutsu repository")))
    (string-trim (buffer-string))))

(defun hunk-notes-jj-working-copy-args ()
  "Return jj arguments for a working-copy diff."
  '("diff"))

(defun hunk-notes-jj-revision-args (revision)
  "Return jj arguments for diffing REVISION."
  (list "diff" "-r" revision))

(defun hunk-notes-jj-range-args (base target)
  "Return jj arguments for diffing BASE to TARGET."
  (list "diff" "--from" base "--to" target))

(defun hunk-notes-jj--run (root args)
  "Run jj in ROOT with ARGS and return stdout as a string."
  (let ((default-directory root))
    (with-temp-buffer
      (let ((status (apply #'process-file "jj" nil t nil args)))
        (unless (zerop status)
          (user-error "jj %s failed:\n%s"
                      (string-join args " ")
                      (buffer-string))))
      (buffer-string))))

(defun hunk-notes-jj--review (root args buffer-name base target diff-id)
  "Create a review buffer from jj ARGS in ROOT."
  (let ((diff (hunk-notes-jj--run root args)))
    (hunk-notes-open-diff-buffer
     buffer-name diff
     :repo-root root
     :backend 'jj
     :base-revision base
     :target-revision target
     :diff-id diff-id)))

;;;###autoload
(defun hunk-notes-jj-review-working-copy ()
  "Review `jj diff' for the current working copy."
  (interactive)
  (let ((root (hunk-notes-jj-root)))
    (hunk-notes-jj--review
     root (hunk-notes-jj-working-copy-args)
     "*hunk-notes jj working copy*" nil "@" "jj-working-copy")))

;;;###autoload
(defun hunk-notes-jj-review-revision (revision)
  "Review jj REVISION with `jj diff -r REVISION'."
  (interactive "sJujutsu revision: ")
  (let ((root (hunk-notes-jj-root)))
    (hunk-notes-jj--review
     root (hunk-notes-jj-revision-args revision)
     (format "*hunk-notes jj %s*" revision)
     nil revision (format "jj-revision:%s" revision))))

;;;###autoload
(defun hunk-notes-jj-review-range (base target)
  "Review jj diff from BASE to TARGET."
  (interactive "sJujutsu base revision: \nsJujutsu target revision: ")
  (let ((root (hunk-notes-jj-root)))
    (hunk-notes-jj--review
     root (hunk-notes-jj-range-args base target)
     (format "*hunk-notes jj %s..%s*" base target)
     base target (format "jj-range:%s..%s" base target))))

(provide 'hunk-notes-jj)
;;; hunk-notes-jj.el ends here
