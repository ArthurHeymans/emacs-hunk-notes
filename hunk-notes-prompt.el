;;; hunk-notes-prompt.el --- Prompt generation for hunk-notes -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Convert local inline diff comments into a structured Markdown prompt for an
;; AI coding agent.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'hunk-notes-core)
(require 'hunk-notes-diff)

(defcustom hunk-notes-prompt-include-full-diff t
  "When non-nil, include the full current diff in generated prompts."
  :type 'boolean
  :group 'hunk-notes)

(defcustom hunk-notes-prompt-diff-scope 'commented-hunks
  "Which diff content to include in generated prompts.
The value `commented-hunks' includes only hunks that have review comments.
The value `full' includes the full diff.  The value `none' omits diff text."
  :type '(choice (const :tag "Only hunks with comments" commented-hunks)
                 (const :tag "Full diff" full)
                 (const :tag "No diff" none))
  :group 'hunk-notes)

(defcustom hunk-notes-prompt-response-format
  "After making changes, summarize what you changed for each review comment. If a comment is not actionable, explain why."
  "Default requested response format for generated prompts."
  :type 'string
  :group 'hunk-notes)

(defun hunk-notes--markdown-code-block (language text)
  "Return TEXT fenced in a Markdown code block labeled LANGUAGE."
  (format "```%s\n%s%s```\n"
          (or language "")
          (or text "")
          (if (and text (not (string-suffix-p "\n" text))) "\n" "")))

(defun hunk-notes-prompt--buffer-substring-without-comment-blocks (beg end)
  "Return buffer text from BEG to END excluding materialized comment blocks."
  (let (chunks)
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (let ((line-end (min end (min (point-max) (1+ (line-end-position))))))
          (unless (get-text-property (line-beginning-position)
                                     'hunk-notes-comment-block)
            (push (buffer-substring-no-properties (point) line-end) chunks)))
        (forward-line 1)))
    (apply #'concat (nreverse chunks))))

(defun hunk-notes-prompt--hunk-region-at-position (pos)
  "Return region covering only the diff hunk at POS."
  (save-excursion
    (goto-char pos)
    (let ((hunk-start (save-excursion
                        (or (re-search-backward "^@@+ " nil t)
                            (point-min)))))
      (goto-char hunk-start)
      (let ((hunk-end (save-excursion
                        (forward-line 1)
                        (or (re-search-forward
                             "^\\(?:@@+ \\|diff --\\|modified +\\|added +\\|removed +\\)"
                             nil t)
                            (point-max)))))
        (when (< hunk-end (point-max))
          (goto-char hunk-end)
          (beginning-of-line)
          (setq hunk-end (point)))
        (cons hunk-start hunk-end)))))

(defun hunk-notes-prompt--commented-hunks-diff-string (comments)
  "Return diff text for hunks associated with COMMENTS."
  (let (chunks seen)
    (dolist (comment comments)
      (let ((pos (and (hunk-notes-comment-file comment)
                      (hunk-notes-comment-line-start comment)
                      (fboundp 'hunk-notes-diff-find-position)
                      (hunk-notes-diff-find-position
                       (hunk-notes-comment-file comment)
                       (hunk-notes-comment-side comment)
                       (hunk-notes-comment-line-start comment)
                       (hunk-notes-comment-change-type comment))))
            (key (list (hunk-notes-comment-file comment)
                       (hunk-notes-comment-hunk-header comment))))
        (when (and pos (not (member key seen)))
          (push key seen)
          (let* ((region (hunk-notes-prompt--hunk-region-at-position pos))
                 (text (hunk-notes-prompt--buffer-substring-without-comment-blocks
                        (car region) (cdr region))))
            (push text chunks)))))
    (mapconcat #'identity (nreverse chunks) "\n")))

(defun hunk-notes-prompt--comment-hunk-key (comment)
  "Return a grouping key for COMMENT's hunk."
  (list (hunk-notes-comment-file comment)
        (hunk-notes-comment-hunk-header comment)))

(defun hunk-notes-prompt--commented-hunk-sections (comments)
  "Return prompt section plists for hunks associated with COMMENTS.
Each plist contains :file, :diff and :comments.  Comments that cannot be
located in the current diff are omitted from the returned sections."
  (let (sections)
    (dolist (comment comments)
      (let* ((key (hunk-notes-prompt--comment-hunk-key comment))
             (cell (assoc key sections)))
        (if cell
            (plist-put (cdr cell) :comments
                       (append (plist-get (cdr cell) :comments)
                               (list comment)))
          (let ((pos (and (hunk-notes-comment-file comment)
                          (hunk-notes-comment-line-start comment)
                          (fboundp 'hunk-notes-diff-find-position)
                          (hunk-notes-diff-find-position
                           (hunk-notes-comment-file comment)
                           (hunk-notes-comment-side comment)
                           (hunk-notes-comment-line-start comment)
                           (hunk-notes-comment-change-type comment)))))
            (when pos
              (let* ((region (hunk-notes-prompt--hunk-region-at-position pos))
                     (text (hunk-notes-prompt--buffer-substring-without-comment-blocks
                            (car region) (cdr region))))
                (push (cons key
                            (list :file (or (hunk-notes-comment-file comment)
                                            "<unknown>")
                                  :diff text
                                  :comments (list comment)))
                      sections)))))))
    (mapcar (lambda (cell)
              (let ((section (cdr cell)))
                (plist-put section :comments
                           (sort (plist-get section :comments)
                                 #'hunk-notes-comment<))
                section))
            (nreverse sections))))

(defun hunk-notes-prompt--comments-in-sections (sections)
  "Return comments included in SECTIONS."
  (apply #'append (mapcar (lambda (section)
                            (plist-get section :comments))
                          sections)))

(defun hunk-notes-prompt--comments-not-in-sections (comments sections)
  "Return COMMENTS not represented in SECTIONS."
  (let ((included (hunk-notes-prompt--comments-in-sections sections)))
    (cl-set-difference comments included :test #'eq)))

(defun hunk-notes-prompt--sections-by-file (sections)
  "Return SECTIONS grouped by file while preserving section order."
  (let (groups)
    (dolist (section sections)
      (let* ((file (or (plist-get section :file) "<unknown>"))
             (cell (assoc file groups)))
        (if cell
            (setcdr cell (append (cdr cell) (list section)))
          (push (list file section) groups))))
    (nreverse groups)))

(defun hunk-notes-prompt--diff-string (comments)
  "Return diff text for COMMENTS according to prompt customization."
  (when hunk-notes-prompt-include-full-diff
    (pcase hunk-notes-prompt-diff-scope
      ('full (hunk-notes--buffer-diff-string))
      ('commented-hunks (hunk-notes-prompt--commented-hunks-diff-string comments))
      ('none nil)
      (_ (hunk-notes-prompt--commented-hunks-diff-string comments)))))

(defun hunk-notes--comments-by-file (&optional comments)
  "Return COMMENTS grouped by file as an alist.
When COMMENTS is nil, use `hunk-notes-comments' from the current buffer."
  (let (groups)
    (dolist (comment (or comments hunk-notes-comments))
      (let* ((file (or (hunk-notes-comment-file comment) "<unknown>"))
             (cell (assoc file groups)))
        (if cell
            (setcdr cell (append (cdr cell) (list comment)))
          (push (list file comment) groups))))
    (nreverse groups)))

(defun hunk-notes--format-comment (comment)
  "Return Markdown for a single COMMENT."
  (let* ((old-start (hunk-notes-comment-safe-old-line-start comment))
         (old-end (hunk-notes-comment-safe-old-line-end comment))
         (new-start (hunk-notes-comment-safe-new-line-start comment))
         (new-end (hunk-notes-comment-safe-new-line-end comment))
         (start (hunk-notes-comment-line-start comment))
         (end (or (hunk-notes-comment-line-end comment) start))
         (range-text (lambda (label from to)
                       (cond
                        ((not from) nil)
                        ((and to (not (= from to)))
                         (format "%s lines %s-%s" label from to))
                        (t (format "%s line %s" label from)))))
         (location
          (cond
           ((and old-start new-start)
            (string-join
             (delq nil (list (funcall range-text "old" old-start old-end)
                             (funcall range-text "new" new-start new-end)))
             "; "))
           (old-start (funcall range-text "old" old-start old-end))
           (new-start (funcall range-text "new" new-start new-end))
           ((not start) "line ?")
           ((and end (not (= start end)))
            (format "%s lines %s-%s" (or (hunk-notes-comment-side comment) 'new)
                    start end))
           (t (format "%s line %s" (or (hunk-notes-comment-side comment) 'new)
                      start)))))
    (format "Comment %s on %s%s:\n%s\n\n"
            (hunk-notes-comment-id comment)
            location
            (if (hunk-notes-comment-resolved comment) " (resolved)" "")
            (or (hunk-notes-comment-body comment) ""))))

(defun hunk-notes-generate-prompt ()
  "Generate a Markdown prompt from the current diff and review comments."
  (interactive)
  (hunk-notes-normalize-comments)
  (let* ((metadata (hunk-notes-current-review-metadata))
         (comments hunk-notes-comments)
         (sections (and (eq hunk-notes-prompt-diff-scope 'commented-hunks)
                        (hunk-notes-prompt--commented-hunk-sections comments)))
         (diff (unless sections
                 (hunk-notes-prompt--diff-string comments))))
    (with-temp-buffer
      (insert "You are addressing review comments on the following code changes.\n\n")
      (insert (format "Repository: %s\n" (or (cdr (assq 'repo-root metadata)) "")))
      (insert (format "Backend: %s\n" (or (cdr (assq 'backend metadata)) "unknown")))
      (insert (format "Base revision: %s\n" (or (cdr (assq 'base-revision metadata)) "")))
      (insert (format "Target revision: %s\n\n" (or (cdr (assq 'target-revision metadata)) "")))
      (insert "## Instructions\n\n")
      (insert "Please address every unresolved review comment. Make concrete code changes where appropriate.\n")
      (insert hunk-notes-prompt-response-format "\n\n")
      (cond
       (sections
        (insert "## Diff and review comments\n\n")
        (dolist (group (hunk-notes-prompt--sections-by-file sections))
          (insert (format "### File: %s\n\n" (car group)))
          (dolist (section (cdr group))
            (insert (hunk-notes--markdown-code-block
                     "diff" (plist-get section :diff)))
            (insert "\nReview comments for this hunk:\n\n")
            (dolist (comment (plist-get section :comments))
              (insert (hunk-notes--format-comment comment)))
            (insert "\n")))
        (let ((unanchored (hunk-notes-prompt--comments-not-in-sections
                           comments sections)))
          (when unanchored
            (insert "## Review comments not located in the current diff\n\n")
            (dolist (group (hunk-notes--comments-by-file unanchored))
              (insert (format "### File: %s\n\n" (car group)))
              (dolist (comment (cdr group))
                (insert (hunk-notes--format-comment comment)))))))
       (diff
        (insert "## Diff\n\n")
        (insert (hunk-notes--markdown-code-block "diff" diff))
        (insert "\n")
        (insert "## Review comments\n\n")
        (if comments
            (dolist (group (hunk-notes--comments-by-file comments))
              (insert (format "### File: %s\n\n" (car group)))
              (dolist (comment (cdr group))
                (insert (hunk-notes--format-comment comment))))
          (insert "No inline review comments were recorded.\n")))
       (t
        (insert "## Review comments\n\n")
        (if comments
            (dolist (group (hunk-notes--comments-by-file comments))
              (insert (format "### File: %s\n\n" (car group)))
              (dolist (comment (cdr group))
                (insert (hunk-notes--format-comment comment))))
          (insert "No inline review comments were recorded.\n"))))
      (buffer-string))))

(defun hunk-notes-show-prompt ()
  "Show the generated review prompt in `*hunk-notes-prompt*'."
  (interactive)
  (let ((prompt (hunk-notes-generate-prompt))
        (buf (get-buffer-create "*hunk-notes-prompt*")))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert prompt)
      (goto-char (point-min))
      (hunk-notes--markdown-mode-if-available)
      (setq buffer-read-only nil))
    (pop-to-buffer buf)))

(defun hunk-notes--markdown-mode-if-available ()
  "Enable `markdown-mode' in the current buffer when available."
  (when (fboundp 'markdown-mode)
    (markdown-mode)))

(defun hunk-notes-copy-prompt ()
  "Copy the generated review prompt to the kill ring."
  (interactive)
  (let ((prompt (hunk-notes-generate-prompt)))
    (kill-new prompt)
    (message "Copied hunk notes prompt (%d characters)" (length prompt))
    prompt))

(defun hunk-notes-insert-prompt ()
  "Insert the generated review prompt at point."
  (interactive)
  (insert (hunk-notes-generate-prompt)))

(defun hunk-notes-save-prompt (file)
  "Save the generated review prompt to Markdown FILE."
  (interactive "FSave review prompt to file: ")
  (write-region (hunk-notes-generate-prompt) nil file)
  (message "Saved review prompt to %s" file)
  file)

(provide 'hunk-notes-prompt)
;;; hunk-notes-prompt.el ends here
