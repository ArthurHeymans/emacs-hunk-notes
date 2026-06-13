;;; hunk-notes-comments.el --- Comment operations for hunk-notes -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Interactive add/edit/delete/navigation commands for inline diff comments.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'hunk-notes-core)
(require 'hunk-notes-diff)

(defvar-local hunk-notes--id-counter 0
  "Buffer-local counter used to generate readable comment IDs for a review.")

(defun hunk-notes--next-comment-id ()
  "Return a new readable review comment id."
  (cl-incf hunk-notes--id-counter)
  (format "R%d" hunk-notes--id-counter))

(defun hunk-notes--sync-id-counter (comments)
  "Advance the buffer-local id counter to cover existing COMMENTS."
  (setq hunk-notes--id-counter 0)
  (dolist (comment comments)
    (let ((id (hunk-notes-comment-id comment)))
      (when (and (stringp id) (string-match "^R\\([0-9]+\\)$" id))
        (setq hunk-notes--id-counter
              (max hunk-notes--id-counter
                   (string-to-number (match-string 1 id))))))))

(defun hunk-notes--renumber-comments (&optional comments)
  "Sort and renumber COMMENTS, defaulting to current buffer comments.
Review ids are intentionally buffer-local display ids.  Keep them sequential in
diff order so prompts and inline blocks read from top to bottom as R1, R2, ..."
  (let ((renumbered (sort (or comments hunk-notes-comments)
                          #'hunk-notes-comment<)))
    (cl-loop for comment in renumbered
             for n from 1
             do (setf (hunk-notes-comment-id comment) (format "R%d" n)))
    (setq hunk-notes-comments renumbered)
    (hunk-notes--sync-id-counter hunk-notes-comments)
    hunk-notes-comments))

(defun hunk-notes--apply-position-info-to-comment (comment info)
  "Update COMMENT anchor fields from diff position INFO."
  (setf (hunk-notes-comment-file comment) (plist-get info :file)
        (hunk-notes-comment-side comment) (plist-get info :side)
        (hunk-notes-comment-line-start comment) (plist-get info :line)
        (hunk-notes-comment-line-end comment) (plist-get info :line)
        (hunk-notes-comment-change-type comment) (plist-get info :change-type)
        (hunk-notes-comment-hunk-header comment) (plist-get info :hunk-header)
        (hunk-notes-comment-anchor-context comment) (plist-get info :anchor-context)
        (hunk-notes-comment-updated-at comment) (hunk-notes--now-string)))

(defun hunk-notes-repair-comment-anchors-from-blocks ()
  "Repair comment file/line anchors from visible inline comment blocks.
This is useful after parser fixes or when older comments were created before a
file header format was understood."
  (interactive)
  (let ((count 0)
        (pos (point-min)))
    (save-excursion
      (while (< pos (point-max))
        (setq pos (or (next-single-property-change
                       pos 'hunk-notes-comment-block nil (point-max))
                      (point-max)))
        (when (and (< pos (point-max))
                   (get-text-property pos 'hunk-notes-comment-block))
          (let ((comment (get-text-property pos 'hunk-notes-comment)))
            (when comment
              (goto-char pos)
              (beginning-of-line)
              (let ((info (hunk-notes-diff-position-info)))
                (when (and info (plist-get info :file))
                  (hunk-notes--apply-position-info-to-comment comment info)
                  (cl-incf count)))))
          (setq pos (or (next-single-property-change
                         pos 'hunk-notes-comment-block nil (point-max))
                        (point-max))))))
    (hunk-notes--sync-id-counter hunk-notes-comments)
    (when (called-interactively-p 'interactive)
      (message "Repaired %d review comment anchors" count))
    count))

(defun hunk-notes--comment-summary (comment)
  "Return a compact display string for COMMENT."
  (format "%s %s:%s %s%s"
          (hunk-notes-comment-id comment)
          (hunk-notes-comment-file comment)
          (hunk-notes-comment-line-start comment)
          (if (hunk-notes-comment-resolved comment) "[resolved] " "")
          (replace-regexp-in-string
           "[\n\r]+" " "
           (truncate-string-to-width (or (hunk-notes-comment-body comment) "") 72))))

(defun hunk-notes--comment-at-point ()
  "Return a comment at point, using text/overlay properties or diff position info."
  (or (get-text-property (point) 'hunk-notes-comment)
      (cl-loop for ov in (overlays-at (point))
               for comment = (overlay-get ov 'hunk-notes-comment)
               when comment return comment)
      (let ((info (hunk-notes-diff-position-info)))
        (when info
          (let* ((matches
                  (cl-remove-if-not
                   (lambda (comment)
                     (and (equal (hunk-notes-comment-file comment)
                                 (plist-get info :file))
                          (eq (hunk-notes-comment-side comment)
                              (plist-get info :side))
                          (= (hunk-notes-comment-line-start comment)
                             (plist-get info :line))))
                   hunk-notes-comments)))
            (cond
             ((null matches) nil)
             ((null (cdr matches)) (car matches))
             (t
              (let* ((choice (completing-read
                              "Comment: "
                              (mapcar #'hunk-notes--comment-summary matches)
                              nil t))
                     (idx (cl-position choice matches
                                       :test #'string=
                                       :key #'hunk-notes--comment-summary)))
                (nth idx matches)))))))))

(defun hunk-notes--read-comment-body (&optional initial prompt)
  "Read a comment body with INITIAL text and PROMPT."
  (read-from-minibuffer (or prompt "Comment: ") initial nil nil nil initial))

(defun hunk-notes--comment-position-info-at-point-or-region ()
  "Return diff position info for point or the active region.
When the region is active, it must cover changed/context lines in one file and
one hunk.  Mixed old/new selections are allowed; the returned info records both
old and new ranges when present."
  (if (use-region-p)
      (let ((beg (region-beginning))
            (end (region-end))
            infos)
        (save-excursion
          (goto-char beg)
          (while (< (line-beginning-position) end)
            (unless (get-text-property (line-beginning-position)
                                       'hunk-notes-comment-block)
              (let ((info (hunk-notes-diff-position-info (point))))
                (when info (push info infos))))
            (forward-line 1)))
        (setq infos (nreverse infos))
        (unless infos
          (user-error "Region does not cover changed or context lines in a unified diff"))
        (let* ((first (car infos))
               (file (plist-get first :file))
               (hunk (plist-get first :hunk-header)))
          (dolist (info infos)
            (unless (and (equal file (plist-get info :file))
                         (equal hunk (plist-get info :hunk-header)))
              (user-error "Region comments must stay within one file and hunk")))
          (let* ((old-lines (delq nil (mapcar (lambda (info)
                                                (plist-get info :old-line))
                                              infos)))
                 (new-lines (delq nil (mapcar (lambda (info)
                                                (plist-get info :new-line))
                                              infos)))
                 ;; Prefer the new side for mixed selections so the visible
                 ;; comment anchors to the post-change hunk when possible.
                 (side (if new-lines 'new 'old))
                 (side-lines (if (eq side 'new) new-lines old-lines))
                 (anchor (or (cl-find-if
                              (lambda (info)
                                (if (eq side 'new)
                                    (plist-get info :new-line)
                                  (plist-get info :old-line)))
                              infos)
                             first))
                 (change-types (delete-dups
                                (delq nil (mapcar (lambda (info)
                                                    (plist-get info :change-type))
                                                  infos)))))
            (append anchor
                    (list :side side
                          :line (apply #'min side-lines)
                          :line-end (apply #'max side-lines)
                          :old-line-start (and old-lines (apply #'min old-lines))
                          :old-line-end (and old-lines (apply #'max old-lines))
                          :new-line-start (and new-lines (apply #'min new-lines))
                          :new-line-end (and new-lines (apply #'max new-lines))
                          :change-type (if (cdr change-types)
                                           'mixed
                                         (car change-types)))))))
    (let ((info (hunk-notes-diff-position-info)))
      (when info
        (append info (list :line-end (plist-get info :line)
                           :old-line-start (plist-get info :old-line)
                           :old-line-end (plist-get info :old-line)
                           :new-line-start (plist-get info :new-line)
                           :new-line-end (plist-get info :new-line)))))))

(defun hunk-notes--new-comment-from-info (info body)
  "Create and add a new comment from diff position INFO with BODY."
  (let* ((now (hunk-notes--now-string))
         (comment
          (hunk-notes-comment-create
           :id (hunk-notes--next-comment-id)
           :repo-root (or hunk-notes-repo-root default-directory)
           :backend hunk-notes-backend
           :base-revision hunk-notes-base-revision
           :target-revision hunk-notes-target-revision
           :file (plist-get info :file)
           :side (plist-get info :side)
           :line-start (plist-get info :line)
           :line-end (or (plist-get info :line-end)
                         (plist-get info :line))
           :old-line-start (plist-get info :old-line-start)
           :old-line-end (plist-get info :old-line-end)
           :new-line-start (plist-get info :new-line-start)
           :new-line-end (plist-get info :new-line-end)
           :change-type (plist-get info :change-type)
           :hunk-header (plist-get info :hunk-header)
           :anchor-context (plist-get info :anchor-context)
           :body body
           :resolved nil
           :created-at now
           :updated-at now)))
    (push comment hunk-notes-comments)
    (hunk-notes--renumber-comments)
    (hunk-notes--maybe-refresh t)
    (message "Added comment %s" (hunk-notes-comment-id comment))
    comment))

(defun hunk-notes-add-comment (body &optional info)
  "Add an inline review comment with BODY at point or active region.
Optional INFO is precomputed diff position information."
  (interactive
   (let ((info (hunk-notes--comment-position-info-at-point-or-region)))
     (list (hunk-notes--read-comment-body nil "Comment: ") info)))
  (let ((info (or info (hunk-notes--comment-position-info-at-point-or-region))))
    (unless info
      (user-error "Point is not on a changed or context line in a unified diff"))
    (when (string-empty-p (string-trim body))
      (user-error "Empty comment"))
    (hunk-notes--new-comment-from-info info body)))

(defun hunk-notes-comment< (a b)
  "Return non-nil when comment A should sort before comment B."
  (let ((fa (or (hunk-notes-comment-file a) ""))
        (fb (or (hunk-notes-comment-file b) "")))
    (or (string< fa fb)
        (and (string= fa fb)
             (< (or (hunk-notes-comment-line-start a) 0)
                (or (hunk-notes-comment-line-start b) 0))))))

(defun hunk-notes-renumber-comments ()
  "Renumber current buffer's review comments sequentially from R1."
  (interactive)
  (hunk-notes--renumber-comments)
  (dolist (comment hunk-notes-comments)
    (setf (hunk-notes-comment-updated-at comment)
          (hunk-notes--now-string)))
  (hunk-notes--maybe-refresh t)
  (message "Renumbered %d review comments" (length hunk-notes-comments)))

(defun hunk-notes-edit-comment (comment body)
  "Edit COMMENT at point, replacing its body with BODY."
  (interactive
   (let ((comment (hunk-notes--comment-at-point)))
     (unless comment (user-error "No review comment at point"))
     (list comment
           (hunk-notes--read-comment-body
            (hunk-notes-comment-body comment)
            (format "Edit %s: " (hunk-notes-comment-id comment))))))
  (if (string-empty-p (string-trim body))
      (progn
        (hunk-notes--delete-comment comment)
        (message "Removed empty comment %s" (hunk-notes-comment-id comment))
        nil)
    (setf (hunk-notes-comment-body comment) body
          (hunk-notes-comment-updated-at comment) (hunk-notes--now-string))
    (hunk-notes--maybe-refresh t)
    (message "Updated comment %s" (hunk-notes-comment-id comment))
    comment))

(defun hunk-notes-comment-dwim (body &optional info)
  "Add or edit a review comment at point using minibuffer BODY.
If point is on an existing comment, update that comment.  Otherwise create a
new comment on the changed/context diff line or active region.  Editing an
existing comment to an empty body removes it."
  (interactive
   (let ((comment (hunk-notes--comment-at-point)))
     (list (hunk-notes--read-comment-body
            (and comment (hunk-notes-comment-body comment))
            (if comment
                (format "Edit %s: " (hunk-notes-comment-id comment))
              "New comment: "))
           (unless comment
             (hunk-notes--comment-position-info-at-point-or-region)))))
  (let ((comment (hunk-notes--comment-at-point)))
    (if comment
        (hunk-notes-edit-comment comment body)
      (hunk-notes-add-comment body info))))

(defvar-local hunk-notes-comment-edit--source-buffer nil
  "Review buffer that owns the comment being edited.")

(defvar-local hunk-notes-comment-edit--comment nil
  "Comment being edited in `hunk-notes-comment-edit-mode'.")

(defvar-local hunk-notes-comment-edit--position-info nil
  "Diff position info for a new comment being edited in block mode.")

(defvar hunk-notes-comment-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'hunk-notes-comment-edit-save)
    (define-key map (kbd "C-c C-k") #'hunk-notes-comment-edit-cancel)
    map)
  "Keymap for `hunk-notes-comment-edit-mode'.")

(define-derived-mode hunk-notes-comment-edit-mode text-mode "Hunk-Notes-Edit"
  "Mode for editing one hunk notes comment body.
Use `C-c C-c' to save the edited comment and `C-c C-k' to cancel.")

(defun hunk-notes--open-comment-edit-buffer (source comment position-info)
  "Open a block edit buffer from SOURCE for COMMENT or POSITION-INFO."
  (let ((buf (get-buffer-create
              (if comment
                  (format "*hunk-notes-edit-%s*"
                          (hunk-notes-comment-id comment))
                "*hunk-notes-new-comment*"))))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (when comment
        (insert (or (hunk-notes-comment-body comment) "")))
      (goto-char (point-min))
      (hunk-notes-comment-edit-mode)
      (setq hunk-notes-comment-edit--source-buffer source
            hunk-notes-comment-edit--comment comment
            hunk-notes-comment-edit--position-info position-info)
      (message "Edit comment, then C-c C-c to save or C-c C-k to cancel"))
    (pop-to-buffer buf)))

(defun hunk-notes-edit-comment-block (comment)
  "Edit COMMENT in a dedicated editable buffer."
  (interactive
   (let ((comment (hunk-notes--comment-at-point)))
     (unless comment (user-error "No review comment at point"))
     (list comment)))
  (hunk-notes--open-comment-edit-buffer (current-buffer) comment nil))

(defun hunk-notes-comment-block-dwim ()
  "Add or edit a review comment in a dedicated block edit buffer.
If point is on an existing comment, edit that comment.  Otherwise start a new
multi-line comment on the changed/context diff line at point.  Saving an empty
edit removes an existing comment or cancels a new one."
  (interactive)
  (let ((comment (hunk-notes--comment-at-point)))
    (if comment
        (hunk-notes--open-comment-edit-buffer (current-buffer) comment nil)
      (let ((info (hunk-notes--comment-position-info-at-point-or-region)))
        (unless info
          (user-error "Point is not on a changed or context line in a unified diff"))
        (hunk-notes--open-comment-edit-buffer (current-buffer) nil info)))))

(defun hunk-notes-comment-edit-save ()
  "Save the current `hunk-notes-comment-edit-mode' buffer back to review."
  (interactive)
  (unless (and hunk-notes-comment-edit--source-buffer
               (buffer-live-p hunk-notes-comment-edit--source-buffer)
               (or hunk-notes-comment-edit--comment
                   hunk-notes-comment-edit--position-info))
    (user-error "This buffer is not connected to a live review comment"))
  (let ((body (string-trim (buffer-substring-no-properties
                            (point-min) (point-max))))
        (source hunk-notes-comment-edit--source-buffer)
        (comment hunk-notes-comment-edit--comment)
        (position-info hunk-notes-comment-edit--position-info))
    (with-current-buffer source
      (if comment
          (hunk-notes-edit-comment comment body)
        (if (string-empty-p body)
            (message "Canceled empty new comment")
          (hunk-notes--new-comment-from-info position-info body))))
    (kill-buffer (current-buffer))
    (pop-to-buffer source)))

(defun hunk-notes-comment-edit-cancel ()
  "Cancel editing the current review comment block."
  (interactive)
  (let ((source hunk-notes-comment-edit--source-buffer))
    (kill-buffer (current-buffer))
    (when (and source (buffer-live-p source))
      (pop-to-buffer source))))

(defun hunk-notes--delete-comment (comment)
  "Delete COMMENT without prompting."
  (setq hunk-notes-comments (delq comment hunk-notes-comments))
  (hunk-notes--renumber-comments)
  (hunk-notes--maybe-refresh t))

(defun hunk-notes-delete-comment (comment)
  "Delete COMMENT at point after confirmation."
  (interactive
   (let ((comment (hunk-notes--comment-at-point)))
     (unless comment (user-error "No review comment at point"))
     (list comment)))
  (when (or noninteractive
            (yes-or-no-p (format "Delete comment %s? "
                                 (hunk-notes-comment-id comment))))
    (hunk-notes--delete-comment comment)
    (message "Deleted comment %s" (hunk-notes-comment-id comment))))

(defun hunk-notes-remove-comment (comment)
  "Remove COMMENT at point without confirmation.
This is the direct remove-this-visible-review-block command."
  (interactive
   (let ((comment (hunk-notes--comment-at-point)))
     (unless comment (user-error "No review comment at point"))
     (list comment)))
  (hunk-notes--delete-comment comment)
  (message "Removed comment %s" (hunk-notes-comment-id comment)))

(defun hunk-notes-toggle-resolved (comment)
  "Toggle resolved state for COMMENT at point."
  (interactive
   (let ((comment (hunk-notes--comment-at-point)))
     (unless comment (user-error "No review comment at point"))
     (list comment)))
  (setf (hunk-notes-comment-resolved comment)
        (not (hunk-notes-comment-resolved comment))
        (hunk-notes-comment-updated-at comment) (hunk-notes--now-string))
  (hunk-notes--maybe-refresh t)
  (message "%s comment %s"
           (if (hunk-notes-comment-resolved comment) "Resolved" "Reopened")
           (hunk-notes-comment-id comment)))

(defun hunk-notes--comment-position (comment)
  "Return buffer position for COMMENT, or nil."
  (hunk-notes-diff-find-position
   (hunk-notes-comment-file comment)
   (hunk-notes-comment-side comment)
   (hunk-notes-comment-line-start comment)
   (hunk-notes-comment-change-type comment)))

(defun hunk-notes-goto-comment (comment)
  "Move point to COMMENT in the current diff buffer."
  (interactive
   (list (let* ((choices (mapcar #'hunk-notes--comment-summary
                                 hunk-notes-comments))
                (choice (completing-read "Comment: " choices nil t))
                (idx (cl-position choice choices :test #'string=)))
           (nth idx hunk-notes-comments))))
  (let ((pos (hunk-notes--comment-position comment)))
    (unless pos
      (user-error "Could not locate comment %s in this diff"
                  (hunk-notes-comment-id comment)))
    (goto-char pos)
    (message "%s" (hunk-notes--comment-summary comment))))

(defun hunk-notes-next-comment ()
  "Jump to the next inline review comment."
  (interactive)
  (unless hunk-notes-comments
    (user-error "No review comments"))
  (let* ((positions
          (sort (cl-loop for comment in hunk-notes-comments
                         for pos = (hunk-notes--comment-position comment)
                         when pos collect (cons pos comment))
                (lambda (a b) (< (car a) (car b)))))
         (next (or (cl-find-if (lambda (cell) (> (car cell) (point))) positions)
                   (car positions))))
    (goto-char (car next))
    (message "%s" (hunk-notes--comment-summary (cdr next)))))

(defun hunk-notes-previous-comment ()
  "Jump to the previous inline review comment."
  (interactive)
  (unless hunk-notes-comments
    (user-error "No review comments"))
  (let* ((positions
          (sort (cl-loop for comment in hunk-notes-comments
                         for pos = (hunk-notes--comment-position comment)
                         when pos collect (cons pos comment))
                (lambda (a b) (< (car a) (car b)))))
         (prev (or (car (last (cl-remove-if-not
                               (lambda (cell) (< (car cell) (point)))
                               positions)))
                   (car (last positions)))))
    (goto-char (car prev))
    (message "%s" (hunk-notes--comment-summary (cdr prev)))))

(defun hunk-notes-list-comments ()
  "Display current review comments in a simple list buffer."
  (interactive)
  (let* ((source (current-buffer))
         (comments (buffer-local-value 'hunk-notes-comments source))
         (buf (get-buffer-create "*hunk-notes-comments*")))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert (format "Review comments for %s\n\n" (buffer-name source)))
      (if comments
          (dolist (comment comments)
            (insert (hunk-notes--comment-summary comment) "\n"
                    (or (hunk-notes-comment-body comment) "") "\n\n"))
        (insert "No comments.\n"))
      (setq buffer-read-only t)
      (special-mode))
    (display-buffer buf)))

(provide 'hunk-notes-comments)
;;; hunk-notes-comments.el ends here
