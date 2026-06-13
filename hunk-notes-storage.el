;;; hunk-notes-storage.el --- Local persistence for hunk-notes -*- lexical-binding: t; -*-

;;; Commentary:

;; Simple sexp-backed local review store.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'hunk-notes-core)

(defcustom hunk-notes-storage-directory
  (locate-user-emacs-file "hunk-notes/reviews/")
  "Directory where local hunk notes files are stored."
  :type 'directory
  :group 'hunk-notes)

(defun hunk-notes-storage-file (&optional key)
  "Return the storage file for review KEY, defaulting to current review key."
  (expand-file-name (concat (or key (hunk-notes-review-key)) ".el")
                    hunk-notes-storage-directory))

(defun hunk-notes--storage-data ()
  "Return the serializable review data for the current buffer."
  `((metadata . ,(hunk-notes-current-review-metadata))
    (comments . ,(mapcar #'hunk-notes-comment-to-plist
                         hunk-notes-comments))))

(defun hunk-notes-save-review (&optional file)
  "Save current review comments to FILE.
Interactively, FILE defaults to the local store file for this review."
  (interactive)
  (let ((file (or file (hunk-notes-storage-file)))
        (data (hunk-notes--storage-data)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (let ((print-length nil)
            (print-level nil))
        (insert ";;; hunk-notes local review -*- mode: emacs-lisp; -*-\n")
        (prin1 data (current-buffer))
        (insert "\n")))
    (when (called-interactively-p 'interactive)
      (message "Saved review to %s" file))
    file))

(defun hunk-notes-load-review (&optional file)
  "Load current review comments from FILE.
Interactively, FILE defaults to the local store file for this review."
  (interactive)
  (let ((file (or file (hunk-notes-storage-file))))
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
                 (mapcar #'hunk-notes-comment-from-plist comments)))))
        (setq hunk-notes-comments loaded-comments)
        (when (fboundp 'hunk-notes--renumber-comments)
          (hunk-notes--renumber-comments hunk-notes-comments))
        (when (fboundp 'hunk-notes--sync-id-counter)
          (hunk-notes--sync-id-counter hunk-notes-comments))))
    (when (fboundp 'hunk-notes-render-comments)
      (hunk-notes-render-comments))
    hunk-notes-comments))

(defun hunk-notes-export-review-json (file)
  "Export the current review as JSON to FILE."
  (interactive "FExport review JSON to file: ")
  (require 'json)
  (let ((json-object-type 'alist)
        (json-array-type 'list)
        (json-key-type 'string)
        (data (hunk-notes--storage-data)))
    (with-temp-file file
      (insert (json-encode data))
      (insert "\n")))
  (message "Exported review JSON to %s" file)
  file)

(defun hunk-notes-list-saved-reviews ()
  "Open `hunk-notes-storage-directory' in Dired."
  (interactive)
  (make-directory hunk-notes-storage-directory t)
  (dired hunk-notes-storage-directory))

(provide 'hunk-notes-storage)
;;; hunk-notes-storage.el ends here
