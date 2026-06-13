;;; hunk-notes-diff.el --- Unified diff parsing for hunk-notes -*- lexical-binding: t; -*-

;;; Commentary:

;; Small, backend-independent helpers for mapping positions in unified diffs to
;; file/line information.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defun hunk-notes-diff--strip-path (path)
  "Normalize PATH from a unified diff header."
  (when path
    (setq path (string-trim path))
    (setq path (car (split-string path "\t")))
    (setq path (car (split-string path " ")))
    (cond
     ((member path '("/dev/null" "")) nil)
     ((string-prefix-p "a/" path) (substring path 2))
     ((string-prefix-p "b/" path) (substring path 2))
     (t path))))

(defun hunk-notes-diff-parse-hunk-header (header)
  "Parse unified or combined diff hunk HEADER.
Return a plist with :old-start, :old-count, :new-start and :new-count, or nil.
Combined diffs, often used by Magit for unmerged files, also include
:combined and :prefix-width."
  (cond
   ((string-match
     "^@@ -\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? +\\+\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? @@"
     header)
    (list :old-start (string-to-number (match-string 1 header))
          :old-count (if (match-string 2 header)
                         (string-to-number (match-string 2 header))
                       1)
          :new-start (string-to-number (match-string 3 header))
          :new-count (if (match-string 4 header)
                         (string-to-number (match-string 4 header))
                       1)
          :prefix-width 1))
   ((string-match
     "^\\(@@@+\\) .* +\\+\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? .*\\1"
     header)
    (list :old-start nil
          :old-count nil
          :new-start (string-to-number (match-string 2 header))
          :new-count (if (match-string 3 header)
                         (string-to-number (match-string 3 header))
                       1)
          :prefix-width (1- (length (match-string 1 header)))
          :combined t))))

(defun hunk-notes-diff--combined-line-info (line prefix-width)
  "Return line info for combined diff LINE with PREFIX-WIDTH columns.
The returned plist contains :change-type, :side, :has-new and :has-old."
  (when (and prefix-width (>= (length line) prefix-width))
    (let* ((prefix (substring line 0 prefix-width))
           (chars (append prefix nil))
           (has-plus (memq ?+ chars))
           (all-minus (cl-every (lambda (char) (= char ?-)) chars))
           (all-space (cl-every (lambda (char) (= char ?\s)) chars)))
      (cond
       (all-minus (list :change-type 'removed :side 'old
                        :has-new nil :has-old t))
       (has-plus (list :change-type 'added :side 'new
                       :has-new t :has-old nil))
       (all-space (list :change-type 'context :side 'new
                        :has-new t :has-old t))
       (t (list :change-type 'context :side 'new
                :has-new t :has-old t))))))

(defun hunk-notes-diff--line-kind ()
  "Return the unified diff kind for the current line."
  (cond
   ((looking-at-p "^@@+ ") 'hunk)
   ((looking-at-p "^diff --git ") 'diff-git)
   ((looking-at-p "^diff --\\(?:cc\\|combined\\) ") 'diff-git)
   ((looking-at-p "^\\(?:modified\\|added\\|removed\\) +") 'file-header)
   ((looking-at-p "^--- ") 'old-file)
   ((looking-at-p "^\\+\\+\\+ ") 'new-file)
   ((looking-at-p "^\\+") 'added)
   ((looking-at-p "^-") 'removed)
   ((looking-at-p "^ ") 'context)
   ((looking-at-p "^\\\\") 'metadata)
   (t 'metadata)))

(defun hunk-notes-diff--line-text ()
  "Return current line as a string without properties."
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(defun hunk-notes-diff--anchor-context ()
  "Return nearby diff text for use as a future re-anchoring hint."
  (save-excursion
    (let ((here (line-beginning-position))
          (lines nil))
      (forward-line -2)
      (dotimes (_ 5)
        (when (and (not (eobp))
                   (not (looking-at-p "^diff --git ")))
          (push (cons (cond ((= (point) here) 'line)
                            ((< (point) here) 'before)
                            (t 'after))
                      (hunk-notes-diff--line-text))
                lines))
        (forward-line 1))
      (nreverse lines))))

(defun hunk-notes-diff--set-file-from-diff-git (line)
  "Return the new-side file name parsed from a diff header LINE."
  (cond
   ((string-match "^diff --git +a/\\(.+?\\) +b/\\(.+\\)$" line)
    (hunk-notes-diff--strip-path (match-string 2 line)))
   ((string-match "^diff --\\(?:cc\\|combined\\) +\\(.+\\)$" line)
    (hunk-notes-diff--strip-path (match-string 1 line)))))

(defun hunk-notes-diff--set-file-from-file-header (line)
  "Return file name from a compact VC diff header LINE.
This handles Jujutsu/Majutsu buffers such as `modified    README.org'."
  (when (string-match "^\\(?:modified\\|added\\|removed\\) +\\(.+\\)$" line)
    (hunk-notes-diff--strip-path (match-string 1 line))))

(defun hunk-notes-diff-position-info (&optional position)
  "Return unified diff information at POSITION, defaulting to point.
The return value is a plist containing :file, :side, :line, :old-line,
:new-line, :change-type, :hunk-header and :anchor-context.  Return nil when
POSITION is not on a changed or context line inside a hunk."
  (let* ((target-pos (or position (point)))
         (target (copy-marker target-pos))
         (target-line-start (save-excursion
                              (goto-char target-pos)
                              (line-beginning-position)))
         file old-file new-file hunk old-line new-line result
         hunk-prefix-width hunk-combined)
    (save-excursion
      (goto-char (point-min))
      (while (and (not result) (<= (point) target) (not (eobp)))
        (let* ((line-start (point))
               (line (hunk-notes-diff--line-text))
               (kind (hunk-notes-diff--line-kind)))
          (pcase kind
            ('diff-git
             (setq file (hunk-notes-diff--set-file-from-diff-git line)
                   old-file nil
                   new-file nil
                   hunk nil
                   old-line nil
                   new-line nil
                   hunk-prefix-width nil
                   hunk-combined nil))
            ('file-header
             (setq file (hunk-notes-diff--set-file-from-file-header line)
                   old-file nil
                   new-file nil
                   hunk nil
                   old-line nil
                   new-line nil
                   hunk-prefix-width nil
                   hunk-combined nil))
            ('old-file
             (setq old-file (hunk-notes-diff--strip-path (substring line 4)))
             (unless file (setq file old-file)))
            ('new-file
             (setq new-file (hunk-notes-diff--strip-path (substring line 4)))
             (when new-file (setq file new-file)))
            ('hunk
             (let ((parsed (hunk-notes-diff-parse-hunk-header line)))
               (if parsed
                   (setq hunk line
                         old-line (plist-get parsed :old-start)
                         new-line (plist-get parsed :new-start)
                         hunk-prefix-width (plist-get parsed :prefix-width)
                         hunk-combined (plist-get parsed :combined))
                 ;; Some diff-like buffers contain lines beginning with "@@"
                 ;; that are not unified diff hunk headers.  Do not leave a
                 ;; half-initialized hunk around; later +/-/context lines must
                 ;; not try to increment nil line counters.
                 (setq hunk nil
                       old-line nil
                       new-line nil
                       hunk-prefix-width nil
                       hunk-combined nil))))
            ((or 'added 'removed 'context 'metadata)
             (if hunk-combined
                 (let ((combined (and (not (eq kind 'metadata))
                                      (hunk-notes-diff--combined-line-info
                                       line hunk-prefix-width))))
                   (when (and combined (= line-start target-line-start))
                     (let* ((side (plist-get combined :side))
                            (change-type (plist-get combined :change-type))
                            (line-number (if (eq side 'old) old-line new-line)))
                       (when line-number
                         (setq result
                               (list :file file :side side :line line-number
                                     :old-line (and (plist-get combined :has-old)
                                                    old-line)
                                     :new-line (and (plist-get combined :has-new)
                                                    new-line)
                                     :change-type change-type :hunk-header hunk
                                     :anchor-context (hunk-notes-diff--anchor-context))))))
                   (when (and combined (plist-get combined :has-old) old-line)
                     (cl-incf old-line))
                   (when (and combined (plist-get combined :has-new) new-line)
                     (cl-incf new-line)))
               (when (and hunk old-line new-line (= line-start target-line-start))
                 (setq result
                       (pcase kind
                         ('added
                          (list :file file :side 'new :line new-line
                                :old-line nil :new-line new-line
                                :change-type 'added :hunk-header hunk
                                :anchor-context (hunk-notes-diff--anchor-context)))
                         ('removed
                          (list :file file :side 'old :line old-line
                                :old-line old-line :new-line nil
                                :change-type 'removed :hunk-header hunk
                                :anchor-context (hunk-notes-diff--anchor-context)))
                         ('context
                          (list :file file :side 'new :line new-line
                                :old-line old-line :new-line new-line
                                :change-type 'context :hunk-header hunk
                                :anchor-context (hunk-notes-diff--anchor-context)))
                         (_ nil))))
               (pcase kind
                 ('added (when new-line (cl-incf new-line)))
                 ('removed (when old-line (cl-incf old-line)))
                 ('context
                  (when old-line (cl-incf old-line))
                  (when new-line (cl-incf new-line)))))))
          (forward-line 1))))
    (set-marker target nil)
    result))

(defun hunk-notes-diff-find-position (file side line &optional change-type)
  "Find buffer position for FILE SIDE LINE in the current diff.
When CHANGE-TYPE is non-nil, prefer an exact change-type match.  Return a
position at the end of the matching diff line, or nil."
  (let (best fallback current-file hunk old-line new-line hunk-prefix-width
             hunk-combined)
    (save-excursion
      (goto-char (point-min))
      (while (and (not best) (not (eobp)))
        (unless (get-text-property (line-beginning-position)
                                   'hunk-notes-comment-block)
          (let* ((text (hunk-notes-diff--line-text))
                 (kind (hunk-notes-diff--line-kind)))
            (pcase kind
              ('diff-git
               (setq current-file (hunk-notes-diff--set-file-from-diff-git text)
                     hunk nil old-line nil new-line nil
                     hunk-prefix-width nil hunk-combined nil))
              ('file-header
               (setq current-file (hunk-notes-diff--set-file-from-file-header text)
                     hunk nil old-line nil new-line nil
                     hunk-prefix-width nil hunk-combined nil))
              ('old-file
               (unless current-file
                 (setq current-file (hunk-notes-diff--strip-path
                                     (substring text 4)))))
              ('new-file
               (let ((new-file (hunk-notes-diff--strip-path
                                (substring text 4))))
                 (when new-file (setq current-file new-file))))
              ('hunk
               (let ((parsed (hunk-notes-diff-parse-hunk-header text)))
                 (if parsed
                     (setq hunk text
                           old-line (plist-get parsed :old-start)
                           new-line (plist-get parsed :new-start)
                           hunk-prefix-width (plist-get parsed :prefix-width)
                           hunk-combined (plist-get parsed :combined))
                   (setq hunk nil old-line nil new-line nil
                         hunk-prefix-width nil hunk-combined nil))))
              ((or 'added 'removed 'context 'metadata)
               (if hunk-combined
                   (let ((combined (and (not (eq kind 'metadata))
                                        (hunk-notes-diff--combined-line-info
                                         text hunk-prefix-width))))
                     (when combined
                       (let* ((this-side (plist-get combined :side))
                              (this-line (if (eq this-side 'old)
                                             old-line
                                           new-line))
                              (this-type (plist-get combined :change-type)))
                         (when (and this-line
                                    (equal file current-file)
                                    (eq side this-side)
                                    (= line this-line))
                           (if (or (not change-type) (eq change-type this-type))
                               (setq best (line-end-position))
                             (unless fallback
                               (setq fallback (line-end-position))))))
                       (when (and (plist-get combined :has-old) old-line)
                         (cl-incf old-line))
                       (when (and (plist-get combined :has-new) new-line)
                         (cl-incf new-line))))
                 (when hunk
                   (pcase kind
                     ('added
                      (when (and new-line
                                 (equal file current-file)
                                 (eq side 'new)
                                 (= line new-line))
                        (if (or (not change-type) (eq change-type 'added))
                            (setq best (line-end-position))
                          (unless fallback
                            (setq fallback (line-end-position)))))
                      (when new-line (cl-incf new-line)))
                     ('removed
                      (when (and old-line
                                 (equal file current-file)
                                 (eq side 'old)
                                 (= line old-line))
                        (if (or (not change-type) (eq change-type 'removed))
                            (setq best (line-end-position))
                          (unless fallback
                            (setq fallback (line-end-position)))))
                      (when old-line (cl-incf old-line)))
                     ('context
                      (when (and new-line
                                 (equal file current-file)
                                 (eq side 'new)
                                 (= line new-line))
                        (if (or (not change-type) (eq change-type 'context))
                            (setq best (line-end-position))
                          (unless fallback
                            (setq fallback (line-end-position)))))
                      (when old-line (cl-incf old-line))
                      (when new-line (cl-incf new-line))))))))))
        (forward-line 1)))
    (or best fallback)))

(defun hunk-notes-diff-current-file ()
  "Return the diff file at point, or nil."
  (plist-get (hunk-notes-diff-position-info) :file))

(provide 'hunk-notes-diff)
;;; hunk-notes-diff.el ends here
