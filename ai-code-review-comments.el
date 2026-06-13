;;; ai-code-review-comments.el --- Comment operations for ai-code-review -*- lexical-binding: t; -*-

;;; Commentary:

;; Interactive add/edit/delete/navigation commands for inline diff comments.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'ai-code-review-core)
(require 'ai-code-review-diff)

(defvar-local ai-code-review--id-counter 0
  "Buffer-local counter used to generate readable comment IDs for a review.")

(defun ai-code-review--next-comment-id ()
  "Return a new readable review comment id."
  (cl-incf ai-code-review--id-counter)
  (format "R%d" ai-code-review--id-counter))

(defun ai-code-review--sync-id-counter (comments)
  "Advance the buffer-local id counter to cover existing COMMENTS."
  (setq ai-code-review--id-counter 0)
  (dolist (comment comments)
    (let ((id (ai-code-review-comment-id comment)))
      (when (and (stringp id) (string-match "^R\\([0-9]+\\)$" id))
        (setq ai-code-review--id-counter
              (max ai-code-review--id-counter
                   (string-to-number (match-string 1 id))))))))

(defun ai-code-review--renumber-comments (&optional comments)
  "Sort and renumber COMMENTS, defaulting to current buffer comments.
Review ids are intentionally buffer-local display ids.  Keep them sequential in
diff order so prompts and inline blocks read from top to bottom as R1, R2, ..."
  (let ((renumbered (sort (or comments ai-code-review-comments)
                          #'ai-code-review-comment<)))
    (cl-loop for comment in renumbered
             for n from 1
             do (setf (ai-code-review-comment-id comment) (format "R%d" n)))
    (setq ai-code-review-comments renumbered)
    (ai-code-review--sync-id-counter ai-code-review-comments)
    ai-code-review-comments))

(defun ai-code-review--apply-position-info-to-comment (comment info)
  "Update COMMENT anchor fields from diff position INFO."
  (setf (ai-code-review-comment-file comment) (plist-get info :file)
        (ai-code-review-comment-side comment) (plist-get info :side)
        (ai-code-review-comment-line-start comment) (plist-get info :line)
        (ai-code-review-comment-line-end comment) (plist-get info :line)
        (ai-code-review-comment-change-type comment) (plist-get info :change-type)
        (ai-code-review-comment-hunk-header comment) (plist-get info :hunk-header)
        (ai-code-review-comment-anchor-context comment) (plist-get info :anchor-context)
        (ai-code-review-comment-updated-at comment) (ai-code-review--now-string)))

(defun ai-code-review-repair-comment-anchors-from-blocks ()
  "Repair comment file/line anchors from visible inline comment blocks.
This is useful after parser fixes or when older comments were created before a
file header format was understood."
  (interactive)
  (let ((count 0)
        (pos (point-min)))
    (save-excursion
      (while (< pos (point-max))
        (setq pos (or (next-single-property-change
                       pos 'ai-code-review-comment-block nil (point-max))
                      (point-max)))
        (when (and (< pos (point-max))
                   (get-text-property pos 'ai-code-review-comment-block))
          (let ((comment (get-text-property pos 'ai-code-review-comment)))
            (when comment
              (goto-char pos)
              (beginning-of-line)
              (let ((info (ai-code-review-diff-position-info)))
                (when (and info (plist-get info :file))
                  (ai-code-review--apply-position-info-to-comment comment info)
                  (cl-incf count)))))
          (setq pos (or (next-single-property-change
                         pos 'ai-code-review-comment-block nil (point-max))
                        (point-max))))))
    (ai-code-review--sync-id-counter ai-code-review-comments)
    (when (called-interactively-p 'interactive)
      (message "Repaired %d review comment anchors" count))
    count))

(defun ai-code-review--comment-summary (comment)
  "Return a compact display string for COMMENT."
  (format "%s %s:%s %s%s"
          (ai-code-review-comment-id comment)
          (ai-code-review-comment-file comment)
          (ai-code-review-comment-line-start comment)
          (if (ai-code-review-comment-resolved comment) "[resolved] " "")
          (replace-regexp-in-string
           "[\n\r]+" " "
           (truncate-string-to-width (or (ai-code-review-comment-body comment) "") 72))))

(defun ai-code-review--comment-at-point ()
  "Return a comment at point, using text/overlay properties or diff position info."
  (or (get-text-property (point) 'ai-code-review-comment)
      (cl-loop for ov in (overlays-at (point))
               for comment = (overlay-get ov 'ai-code-review-comment)
               when comment return comment)
      (let ((info (ai-code-review-diff-position-info)))
        (when info
          (let* ((matches
                  (cl-remove-if-not
                   (lambda (comment)
                     (and (equal (ai-code-review-comment-file comment)
                                 (plist-get info :file))
                          (eq (ai-code-review-comment-side comment)
                              (plist-get info :side))
                          (= (ai-code-review-comment-line-start comment)
                             (plist-get info :line))))
                   ai-code-review-comments)))
            (cond
             ((null matches) nil)
             ((null (cdr matches)) (car matches))
             (t
              (let* ((choice (completing-read
                              "Comment: "
                              (mapcar #'ai-code-review--comment-summary matches)
                              nil t))
                     (idx (cl-position choice matches
                                       :test #'string=
                                       :key #'ai-code-review--comment-summary)))
                (nth idx matches)))))))))

(defun ai-code-review--read-comment-body (&optional initial prompt)
  "Read a comment body with INITIAL text and PROMPT."
  (read-from-minibuffer (or prompt "Comment: ") initial nil nil nil initial))

(defun ai-code-review--comment-position-info-at-point-or-region ()
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
                                       'ai-code-review-comment-block)
              (let ((info (ai-code-review-diff-position-info (point))))
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
    (let ((info (ai-code-review-diff-position-info)))
      (when info
        (append info (list :line-end (plist-get info :line)
                           :old-line-start (plist-get info :old-line)
                           :old-line-end (plist-get info :old-line)
                           :new-line-start (plist-get info :new-line)
                           :new-line-end (plist-get info :new-line)))))))

(defun ai-code-review--new-comment-from-info (info body)
  "Create and add a new comment from diff position INFO with BODY."
  (let* ((now (ai-code-review--now-string))
         (comment
          (ai-code-review-comment-create
           :id (ai-code-review--next-comment-id)
           :repo-root (or ai-code-review-repo-root default-directory)
           :backend ai-code-review-backend
           :base-revision ai-code-review-base-revision
           :target-revision ai-code-review-target-revision
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
    (push comment ai-code-review-comments)
    (ai-code-review--renumber-comments)
    (ai-code-review--maybe-refresh t)
    (message "Added comment %s" (ai-code-review-comment-id comment))
    comment))

(defun ai-code-review-add-comment (body &optional info)
  "Add an inline review comment with BODY at point or active region.
Optional INFO is precomputed diff position information."
  (interactive
   (let ((info (ai-code-review--comment-position-info-at-point-or-region)))
     (list (ai-code-review--read-comment-body nil "Comment: ") info)))
  (let ((info (or info (ai-code-review--comment-position-info-at-point-or-region))))
    (unless info
      (user-error "Point is not on a changed or context line in a unified diff"))
    (when (string-empty-p (string-trim body))
      (user-error "Empty comment"))
    (ai-code-review--new-comment-from-info info body)))

(defun ai-code-review-comment< (a b)
  "Return non-nil when comment A should sort before comment B."
  (let ((fa (or (ai-code-review-comment-file a) ""))
        (fb (or (ai-code-review-comment-file b) "")))
    (or (string< fa fb)
        (and (string= fa fb)
             (< (or (ai-code-review-comment-line-start a) 0)
                (or (ai-code-review-comment-line-start b) 0))))))

(defun ai-code-review-renumber-comments ()
  "Renumber current buffer's review comments sequentially from R1."
  (interactive)
  (ai-code-review--renumber-comments)
  (dolist (comment ai-code-review-comments)
    (setf (ai-code-review-comment-updated-at comment)
          (ai-code-review--now-string)))
  (ai-code-review--maybe-refresh t)
  (message "Renumbered %d review comments" (length ai-code-review-comments)))

(defun ai-code-review-edit-comment (comment body)
  "Edit COMMENT at point, replacing its body with BODY."
  (interactive
   (let ((comment (ai-code-review--comment-at-point)))
     (unless comment (user-error "No review comment at point"))
     (list comment
           (ai-code-review--read-comment-body
            (ai-code-review-comment-body comment)
            (format "Edit %s: " (ai-code-review-comment-id comment))))))
  (if (string-empty-p (string-trim body))
      (progn
        (ai-code-review--delete-comment comment)
        (message "Removed empty comment %s" (ai-code-review-comment-id comment))
        nil)
    (setf (ai-code-review-comment-body comment) body
          (ai-code-review-comment-updated-at comment) (ai-code-review--now-string))
    (ai-code-review--maybe-refresh t)
    (message "Updated comment %s" (ai-code-review-comment-id comment))
    comment))

(defun ai-code-review-comment-dwim (body &optional info)
  "Add or edit a review comment at point using minibuffer BODY.
If point is on an existing comment, update that comment.  Otherwise create a
new comment on the changed/context diff line or active region.  Editing an
existing comment to an empty body removes it."
  (interactive
   (let ((comment (ai-code-review--comment-at-point)))
     (list (ai-code-review--read-comment-body
            (and comment (ai-code-review-comment-body comment))
            (if comment
                (format "Edit %s: " (ai-code-review-comment-id comment))
              "New comment: "))
           (unless comment
             (ai-code-review--comment-position-info-at-point-or-region)))))
  (let ((comment (ai-code-review--comment-at-point)))
    (if comment
        (ai-code-review-edit-comment comment body)
      (ai-code-review-add-comment body info))))

(defvar-local ai-code-review-comment-edit--source-buffer nil
  "Review buffer that owns the comment being edited.")

(defvar-local ai-code-review-comment-edit--comment nil
  "Comment being edited in `ai-code-review-comment-edit-mode'.")

(defvar-local ai-code-review-comment-edit--position-info nil
  "Diff position info for a new comment being edited in block mode.")

(defvar ai-code-review-comment-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'ai-code-review-comment-edit-save)
    (define-key map (kbd "C-c C-k") #'ai-code-review-comment-edit-cancel)
    map)
  "Keymap for `ai-code-review-comment-edit-mode'.")

(define-derived-mode ai-code-review-comment-edit-mode text-mode "AI-Review-Edit"
  "Mode for editing one AI code review comment body.
Use `C-c C-c' to save the edited comment and `C-c C-k' to cancel.")

(defun ai-code-review--open-comment-edit-buffer (source comment position-info)
  "Open a block edit buffer from SOURCE for COMMENT or POSITION-INFO."
  (let ((buf (get-buffer-create
              (if comment
                  (format "*ai-code-review-edit-%s*"
                          (ai-code-review-comment-id comment))
                "*ai-code-review-new-comment*"))))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (when comment
        (insert (or (ai-code-review-comment-body comment) "")))
      (goto-char (point-min))
      (ai-code-review-comment-edit-mode)
      (setq ai-code-review-comment-edit--source-buffer source
            ai-code-review-comment-edit--comment comment
            ai-code-review-comment-edit--position-info position-info)
      (message "Edit comment, then C-c C-c to save or C-c C-k to cancel"))
    (pop-to-buffer buf)))

(defun ai-code-review-edit-comment-block (comment)
  "Edit COMMENT in a dedicated editable buffer."
  (interactive
   (let ((comment (ai-code-review--comment-at-point)))
     (unless comment (user-error "No review comment at point"))
     (list comment)))
  (ai-code-review--open-comment-edit-buffer (current-buffer) comment nil))

(defun ai-code-review-comment-block-dwim ()
  "Add or edit a review comment in a dedicated block edit buffer.
If point is on an existing comment, edit that comment.  Otherwise start a new
multi-line comment on the changed/context diff line at point.  Saving an empty
edit removes an existing comment or cancels a new one."
  (interactive)
  (let ((comment (ai-code-review--comment-at-point)))
    (if comment
        (ai-code-review--open-comment-edit-buffer (current-buffer) comment nil)
      (let ((info (ai-code-review--comment-position-info-at-point-or-region)))
        (unless info
          (user-error "Point is not on a changed or context line in a unified diff"))
        (ai-code-review--open-comment-edit-buffer (current-buffer) nil info)))))

(defun ai-code-review-comment-edit-save ()
  "Save the current `ai-code-review-comment-edit-mode' buffer back to review."
  (interactive)
  (unless (and ai-code-review-comment-edit--source-buffer
               (buffer-live-p ai-code-review-comment-edit--source-buffer)
               (or ai-code-review-comment-edit--comment
                   ai-code-review-comment-edit--position-info))
    (user-error "This buffer is not connected to a live review comment"))
  (let ((body (string-trim (buffer-substring-no-properties
                            (point-min) (point-max))))
        (source ai-code-review-comment-edit--source-buffer)
        (comment ai-code-review-comment-edit--comment)
        (position-info ai-code-review-comment-edit--position-info))
    (with-current-buffer source
      (if comment
          (ai-code-review-edit-comment comment body)
        (if (string-empty-p body)
            (message "Canceled empty new comment")
          (ai-code-review--new-comment-from-info position-info body))))
    (kill-buffer (current-buffer))
    (pop-to-buffer source)))

(defun ai-code-review-comment-edit-cancel ()
  "Cancel editing the current review comment block."
  (interactive)
  (let ((source ai-code-review-comment-edit--source-buffer))
    (kill-buffer (current-buffer))
    (when (and source (buffer-live-p source))
      (pop-to-buffer source))))

(defun ai-code-review--delete-comment (comment)
  "Delete COMMENT without prompting."
  (setq ai-code-review-comments (delq comment ai-code-review-comments))
  (ai-code-review--renumber-comments)
  (ai-code-review--maybe-refresh t))

(defun ai-code-review-delete-comment (comment)
  "Delete COMMENT at point after confirmation."
  (interactive
   (let ((comment (ai-code-review--comment-at-point)))
     (unless comment (user-error "No review comment at point"))
     (list comment)))
  (when (or noninteractive
            (yes-or-no-p (format "Delete comment %s? "
                                 (ai-code-review-comment-id comment))))
    (ai-code-review--delete-comment comment)
    (message "Deleted comment %s" (ai-code-review-comment-id comment))))

(defun ai-code-review-remove-comment (comment)
  "Remove COMMENT at point without confirmation.
This is the direct remove-this-visible-review-block command."
  (interactive
   (let ((comment (ai-code-review--comment-at-point)))
     (unless comment (user-error "No review comment at point"))
     (list comment)))
  (ai-code-review--delete-comment comment)
  (message "Removed comment %s" (ai-code-review-comment-id comment)))

(defun ai-code-review-toggle-resolved (comment)
  "Toggle resolved state for COMMENT at point."
  (interactive
   (let ((comment (ai-code-review--comment-at-point)))
     (unless comment (user-error "No review comment at point"))
     (list comment)))
  (setf (ai-code-review-comment-resolved comment)
        (not (ai-code-review-comment-resolved comment))
        (ai-code-review-comment-updated-at comment) (ai-code-review--now-string))
  (ai-code-review--maybe-refresh t)
  (message "%s comment %s"
           (if (ai-code-review-comment-resolved comment) "Resolved" "Reopened")
           (ai-code-review-comment-id comment)))

(defun ai-code-review--comment-position (comment)
  "Return buffer position for COMMENT, or nil."
  (ai-code-review-diff-find-position
   (ai-code-review-comment-file comment)
   (ai-code-review-comment-side comment)
   (ai-code-review-comment-line-start comment)
   (ai-code-review-comment-change-type comment)))

(defun ai-code-review-goto-comment (comment)
  "Move point to COMMENT in the current diff buffer."
  (interactive
   (list (let* ((choices (mapcar #'ai-code-review--comment-summary
                                 ai-code-review-comments))
                (choice (completing-read "Comment: " choices nil t))
                (idx (cl-position choice choices :test #'string=)))
           (nth idx ai-code-review-comments))))
  (let ((pos (ai-code-review--comment-position comment)))
    (unless pos
      (user-error "Could not locate comment %s in this diff"
                  (ai-code-review-comment-id comment)))
    (goto-char pos)
    (message "%s" (ai-code-review--comment-summary comment))))

(defun ai-code-review-next-comment ()
  "Jump to the next inline review comment."
  (interactive)
  (unless ai-code-review-comments
    (user-error "No review comments"))
  (let* ((positions
          (sort (cl-loop for comment in ai-code-review-comments
                         for pos = (ai-code-review--comment-position comment)
                         when pos collect (cons pos comment))
                (lambda (a b) (< (car a) (car b)))))
         (next (or (cl-find-if (lambda (cell) (> (car cell) (point))) positions)
                   (car positions))))
    (goto-char (car next))
    (message "%s" (ai-code-review--comment-summary (cdr next)))))

(defun ai-code-review-previous-comment ()
  "Jump to the previous inline review comment."
  (interactive)
  (unless ai-code-review-comments
    (user-error "No review comments"))
  (let* ((positions
          (sort (cl-loop for comment in ai-code-review-comments
                         for pos = (ai-code-review--comment-position comment)
                         when pos collect (cons pos comment))
                (lambda (a b) (< (car a) (car b)))))
         (prev (or (car (last (cl-remove-if-not
                               (lambda (cell) (< (car cell) (point)))
                               positions)))
                   (car (last positions)))))
    (goto-char (car prev))
    (message "%s" (ai-code-review--comment-summary (cdr prev)))))

(defun ai-code-review-list-comments ()
  "Display current review comments in a simple list buffer."
  (interactive)
  (let* ((source (current-buffer))
         (comments (buffer-local-value 'ai-code-review-comments source))
         (buf (get-buffer-create "*ai-code-review-comments*")))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert (format "Review comments for %s\n\n" (buffer-name source)))
      (if comments
          (dolist (comment comments)
            (insert (ai-code-review--comment-summary comment) "\n"
                    (or (ai-code-review-comment-body comment) "") "\n\n"))
        (insert "No comments.\n"))
      (setq buffer-read-only t)
      (special-mode))
    (display-buffer buf)))

(provide 'ai-code-review-comments)
;;; ai-code-review-comments.el ends here
