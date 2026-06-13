;;; ai-code-review-storage.el --- Local persistence for ai-code-review -*- lexical-binding: t; -*-

;;; Commentary:

;; Simple sexp-backed local review store.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'ai-code-review-core)

(defcustom ai-code-review-storage-directory
  (locate-user-emacs-file "ai-code-review/reviews/")
  "Directory where local AI code review files are stored."
  :type 'directory
  :group 'ai-code-review)

(defun ai-code-review-storage-file (&optional key)
  "Return the storage file for review KEY, defaulting to current review key."
  (expand-file-name (concat (or key (ai-code-review-review-key)) ".el")
                    ai-code-review-storage-directory))

(defun ai-code-review--storage-data ()
  "Return the serializable review data for the current buffer."
  `((metadata . ,(ai-code-review-current-review-metadata))
    (comments . ,(mapcar #'ai-code-review-comment-to-plist
                         ai-code-review-comments))))

(defun ai-code-review-save-review (&optional file)
  "Save current review comments to FILE.
Interactively, FILE defaults to the local store file for this review."
  (interactive)
  (let ((file (or file (ai-code-review-storage-file)))
        (data (ai-code-review--storage-data)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (let ((print-length nil)
            (print-level nil))
        (insert ";;; ai-code-review local review -*- mode: emacs-lisp; -*-\n")
        (prin1 data (current-buffer))
        (insert "\n")))
    (when (called-interactively-p 'interactive)
      (message "Saved review to %s" file))
    file))

(defun ai-code-review-load-review (&optional file)
  "Load current review comments from FILE.
Interactively, FILE defaults to the local store file for this review."
  (interactive)
  (let ((file (or file (ai-code-review-storage-file))))
    (if (not (file-exists-p file))
        (when (called-interactively-p 'interactive)
          (message "No saved review at %s" file))
      (let ((loaded-comments
             (with-temp-buffer
               (insert-file-contents file)
               (goto-char (point-min))
               (when (looking-at-p ";")
                 (forward-line 1))
               (let* ((data (read (current-buffer)))
                      (comments (cdr (assq 'comments data))))
                 (mapcar #'ai-code-review-comment-from-plist comments)))))
        (setq ai-code-review-comments loaded-comments)
        (when (fboundp 'ai-code-review--sync-id-counter)
          (ai-code-review--sync-id-counter ai-code-review-comments))))
    (when (fboundp 'ai-code-review-render-comments)
      (ai-code-review-render-comments))
    ai-code-review-comments))

(defun ai-code-review-export-review-json (file)
  "Export the current review as JSON to FILE."
  (interactive "FExport review JSON to file: ")
  (require 'json)
  (let ((json-object-type 'alist)
        (json-array-type 'list)
        (json-key-type 'string)
        (data (ai-code-review--storage-data)))
    (with-temp-file file
      (insert (json-encode data))
      (insert "\n")))
  (message "Exported review JSON to %s" file)
  file)

(defun ai-code-review-list-saved-reviews ()
  "Open `ai-code-review-storage-directory' in Dired."
  (interactive)
  (make-directory ai-code-review-storage-directory t)
  (dired ai-code-review-storage-directory))

(provide 'ai-code-review-storage)
;;; ai-code-review-storage.el ends here
