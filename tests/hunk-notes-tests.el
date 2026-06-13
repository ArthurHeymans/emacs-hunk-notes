;;; hunk-notes-tests.el --- Tests for hunk-notes -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path (file-name-directory (directory-file-name
                                             (file-name-directory load-file-name))))
(require 'hunk-notes)
(require 'hunk-notes-git)
(require 'hunk-notes-jj)

(defconst hunk-notes-test-fixture
  (expand-file-name "fixtures/simple.diff" (file-name-directory load-file-name)))

(defun hunk-notes-test--with-fixture (fn)
  (with-temp-buffer
    (insert-file-contents hunk-notes-test-fixture)
    (diff-mode)
    (let ((hunk-notes-auto-save nil)
          (hunk-notes--id-counter 0))
      (hunk-notes-start :repo-root "/tmp/repo" :backend 'git
                            :target-revision "working-copy"
                            :diff-id "test")
      (funcall fn))))

(defun hunk-notes-test--string-count (needle string)
  "Return number of non-overlapping NEEDLE matches in STRING."
  (let ((count 0)
        (start 0))
    (while (string-match (regexp-quote needle) string start)
      (setq start (match-end 0)
            count (1+ count)))
    count))

(ert-deftest hunk-notes-diff-parse-hunk-header ()
  (should (equal (hunk-notes-diff-parse-hunk-header "@@ -10,7 +10,9 @@")
                 '(:old-start 10 :old-count 7 :new-start 10 :new-count 9
                   :prefix-width 1)))
  (should (equal (hunk-notes-diff-parse-hunk-header "@@ -1 +2 @@")
                 '(:old-start 1 :old-count 1 :new-start 2 :new-count 1
                   :prefix-width 1)))
  (let ((combined (hunk-notes-diff-parse-hunk-header
                   "@@@ -1,3 -1,4 +1,5 @@@")))
    (should (plist-get combined :combined))
    (should (= (plist-get combined :new-start) 1))
    (should (= (plist-get combined :prefix-width) 2))))

(ert-deftest hunk-notes-diff-position-added-line ()
  (hunk-notes-test--with-fixture
   (lambda ()
     (goto-char (point-min))
     (search-forward "+  (message \"new\")")
     (let ((info (hunk-notes-diff-position-info)))
       (should (equal (plist-get info :file) "src/foo.el"))
       (should (eq (plist-get info :side) 'new))
       (should (= (plist-get info :line) 2))
       (should (eq (plist-get info :change-type) 'added))))))

(ert-deftest hunk-notes-diff-position-removed-line ()
  (hunk-notes-test--with-fixture
   (lambda ()
     (goto-char (point-min))
     (search-forward "-  (message \"old\")")
     (let ((info (hunk-notes-diff-position-info)))
       (should (equal (plist-get info :file) "src/foo.el"))
       (should (eq (plist-get info :side) 'old))
       (should (= (plist-get info :line) 2))
       (should (eq (plist-get info :change-type) 'removed))))))

(ert-deftest hunk-notes-diff-ignores-non-hunk-plus-lines ()
  (with-temp-buffer
    (insert "diff --git a/foo b/foo\n"
            "+not a real changed line yet\n"
            "@@ this is not a valid hunk header @@\n"
            "+still not in a parsed hunk\n")
    (diff-mode)
    (goto-char (point-min))
    (search-forward "+still")
    (should-not (hunk-notes-diff-position-info))))

(ert-deftest hunk-notes-diff-position-combined-added-line ()
  (with-temp-buffer
    (insert "diff --cc src/foo.el\n"
            "index 1111111,2222222..3333333\n"
            "--- a/src/foo.el\n"
            "+++ b/src/foo.el\n"
            "@@@ -1,3 -1,3 +1,4 @@@\n"
            "  context\n"
            "++added in result\n"
            "--removed from both parents\n")
    (diff-mode)
    (goto-char (point-min))
    (search-forward "++added")
    (let ((info (hunk-notes-diff-position-info)))
      (should (equal (plist-get info :file) "src/foo.el"))
      (should (eq (plist-get info :side) 'new))
      (should (= (plist-get info :line) 2))
      (should (eq (plist-get info :change-type) 'added)))))

(ert-deftest hunk-notes-diff-position-jj-compact-file-header ()
  (with-temp-buffer
    (insert "modified    README.org\n"
            "@@ -0,1 +1,3 @@\n"
            "+one\n"
            "+two\n")
    (diff-mode)
    (goto-char (point-min))
    (search-forward "+two")
    (let ((info (hunk-notes-diff-position-info)))
      (should (equal (plist-get info :file) "README.org"))
      (should (eq (plist-get info :side) 'new))
      (should (= (plist-get info :line) 2))
      (should (eq (plist-get info :change-type) 'added)))))

(ert-deftest hunk-notes-comments-add-edit-delete ()
  (hunk-notes-test--with-fixture
   (lambda ()
     (goto-char (point-min))
     (search-forward "+  (message \"new\")")
     (let ((comment (hunk-notes-add-comment "Please rename this message.")))
       (should (= (length hunk-notes-comments) 1))
       (should (equal (hunk-notes-comment-id comment) "R1"))
       (hunk-notes-edit-comment comment "Updated body")
       (should (equal (hunk-notes-comment-body comment) "Updated body"))
       (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
         (hunk-notes-delete-comment comment))
       (should (null hunk-notes-comments))))))

(ert-deftest hunk-notes-empty-edit-removes-comment ()
  (hunk-notes-test--with-fixture
   (lambda ()
     (goto-char (point-min))
     (search-forward "+  (message \"new\")")
     (let ((comment (hunk-notes-add-comment "Temporary")))
       (should (= (length hunk-notes-comments) 1))
       (hunk-notes-edit-comment comment "   ")
       (should (null hunk-notes-comments))))))

(ert-deftest hunk-notes-renumbers-comments-in-diff-order ()
  (hunk-notes-test--with-fixture
   (lambda ()
     (let ((hunk-notes-render-style 'overlay))
       (goto-char (point-min))
       (search-forward "+;; added")
       (let ((lower (hunk-notes-add-comment "Lower hunk")))
         (should (equal (hunk-notes-comment-id lower) "R1"))
         (goto-char (point-min))
         (search-forward "+  (message \"new\")")
         (let ((upper (hunk-notes-add-comment "Upper hunk")))
           (should (equal (hunk-notes-comment-id upper) "R1"))
           (should (equal (hunk-notes-comment-id lower) "R2"))
           (hunk-notes-remove-comment upper)
           (should (equal (hunk-notes-comment-id lower) "R1"))))))))

(ert-deftest hunk-notes-region-comment-line-range ()
  (hunk-notes-test--with-fixture
   (lambda ()
     (goto-char (point-min))
     (search-forward "+  (message \"new\")")
     (beginning-of-line)
     (push-mark (point) t t)
     (setq mark-active t)
     (forward-line 2)
     (let* ((transient-mark-mode t)
            (mark-even-if-inactive t)
            (comment (hunk-notes-comment-dwim "Two-line comment")))
       (should (= (hunk-notes-comment-line-start comment) 2))
       (should (= (hunk-notes-comment-line-end comment) 3))
       (should (string-match-p "lines 2-3"
                               (hunk-notes-generate-prompt)))))))

(ert-deftest hunk-notes-region-comment-allows-mixed-side-hunk-block ()
  (hunk-notes-test--with-fixture
   (lambda ()
     (goto-char (point-min))
     (search-forward "-  (message \"old\")")
     (beginning-of-line)
     (push-mark (point) t t)
     (setq mark-active t)
     (forward-line 3)
     (let* ((transient-mark-mode t)
            (mark-even-if-inactive t)
            (comment (hunk-notes-comment-dwim "Replacement block comment")))
       (should (eq (hunk-notes-comment-side comment) 'new))
       (should (= (hunk-notes-comment-old-line-start comment) 2))
       (should (= (hunk-notes-comment-old-line-end comment) 2))
       (should (= (hunk-notes-comment-new-line-start comment) 2))
       (should (= (hunk-notes-comment-new-line-end comment) 3))
       (let ((prompt (hunk-notes-generate-prompt)))
         (should (string-match-p "old line 2; new lines 2-3" prompt))
         (should (string-match-p "Replacement block comment" prompt)))))))

(ert-deftest hunk-notes-prompt-generation ()
  (hunk-notes-test--with-fixture
   (lambda ()
     (goto-char (point-min))
     (search-forward "+  (message \"extra\")")
     (hunk-notes-add-comment "This extra message should be justified.")
     (let ((prompt (hunk-notes-generate-prompt)))
       (should (string-match-p "Repository: /tmp/repo" prompt))
       (should (string-match-p "## Diff and review comments" prompt))
       (should (string-match-p "Comment R" prompt))
       (should (string-match-p "This extra message" prompt))))))

(ert-deftest hunk-notes-prompt-places-comments-next-to-hunks ()
  (hunk-notes-test--with-fixture
   (lambda ()
     (goto-char (point-min))
     (search-forward "+  (message \"extra\")")
     (hunk-notes-add-comment "Comment near first hunk")
     (let* ((prompt (hunk-notes-generate-prompt))
            (hunk-pos (string-match-p (regexp-quote "@@ -1,5 +1,6 @@")
                                      prompt))
            (comment-pos (string-match-p "Comment R1 on new line 3" prompt)))
       (should hunk-pos)
       (should comment-pos)
       (should (< hunk-pos comment-pos))
       (should (string-match-p "Review comments for this hunk" prompt))
       (should-not (string-match-p "## Review comments\n\n### File" prompt))))))

(ert-deftest hunk-notes-prompt-does-not-include-earlier-hunks-for-same-file ()
  (hunk-notes-test--with-fixture
   (lambda ()
     (goto-char (point-min))
     (search-forward "+;; added")
     (hunk-notes-add-comment "Second hunk only")
     (let ((prompt (hunk-notes-generate-prompt)))
       (should (string-match-p (regexp-quote "@@ -20,3 +21,4 @@") prompt))
       (should-not (string-match-p (regexp-quote "@@ -1,5 +1,6 @@") prompt))
       (should-not (string-match-p (regexp-quote "+  (message \"new\")")
                                   prompt))))))

(ert-deftest hunk-notes-prompt-groups-multiple-commented-hunks-by-file ()
  (hunk-notes-test--with-fixture
   (lambda ()
     (goto-char (point-min))
     (search-forward "+  (message \"extra\")")
     (hunk-notes-add-comment "First hunk comment")
     (goto-char (point-min))
     (search-forward "+;; added")
     (hunk-notes-add-comment "Second hunk comment")
     (let ((prompt (hunk-notes-generate-prompt)))
       (should (= (hunk-notes-test--string-count "### File: src/foo.el"
                                                     prompt)
                  1))
       (should (string-match-p (regexp-quote "@@ -1,5 +1,6 @@") prompt))
       (should (string-match-p (regexp-quote "@@ -20,3 +21,4 @@") prompt))
       (should (string-match-p "First hunk comment" prompt))
       (should (string-match-p "Second hunk comment" prompt))
       (should (= (hunk-notes-test--string-count
                   "Review comments for this hunk" prompt)
                  2))))))

(ert-deftest hunk-notes-storage-roundtrip ()
  (hunk-notes-test--with-fixture
   (lambda ()
     (let ((hunk-notes-storage-directory
            (make-temp-file "hunk-notes-tests" t)))
       (goto-char (point-min))
       (search-forward "+  (message \"new\")")
       (hunk-notes-add-comment "Persist me")
       (let ((file (hunk-notes-save-review)))
         (setq hunk-notes-comments nil)
         (hunk-notes-load-review file)
         (should (= (length hunk-notes-comments) 1))
         (should (equal (hunk-notes-comment-body (car hunk-notes-comments))
                        "Persist me")))))))

(ert-deftest hunk-notes-command-construction ()
  (should (equal (hunk-notes-git-working-copy-args)
                 '("diff" "--no-ext-diff" "--")))
  (should (equal (hunk-notes-git-range-args "main" "HEAD")
                 '("diff" "--no-ext-diff" "main" "HEAD" "--")))
  (should (equal (hunk-notes-jj-range-args "main" "@")
                 '("diff" "--from" "main" "--to" "@"))))

(ert-deftest hunk-notes-auto-enable-hides-comments-and-uses-overlay-style ()
  (let ((hunk-notes-auto-enable-in-diff-buffers t)
        (hunk-notes-auto-enable-show-comments nil)
        (hunk-notes-auto-enable-render-style 'overlay)
        (hunk-notes-auto-save nil))
    (with-temp-buffer
      (insert-file-contents hunk-notes-test-fixture)
      (diff-mode)
      (should hunk-notes-mode)
      (should hunk-notes--auto-enabled)
      (should-not hunk-notes-comments-visible)
      (should (eq hunk-notes-render-style 'overlay)))))

(ert-deftest hunk-notes-open-diff-buffer-preserves-start-metadata ()
  (let ((hunk-notes-auto-save nil)
        (name "*hunk-notes-test-open-diff-buffer*"))
    (unwind-protect
        (let ((buf (hunk-notes-open-diff-buffer
                    name
                    (with-temp-buffer
                      (insert-file-contents hunk-notes-test-fixture)
                      (buffer-string))
                    :repo-root "/tmp/repo"
                    :backend 'git
                    :target-revision "working-copy"
                    :diff-id "open-test"
                    :comments-visible nil)))
          (with-current-buffer buf
            (should (eq major-mode 'diff-mode))
            (should hunk-notes-mode)
            (should-not hunk-notes-comments-visible)
            (should (equal hunk-notes-repo-root "/tmp/repo"))
            (should (eq hunk-notes-backend 'git))
            (should (equal hunk-notes-target-revision "working-copy"))
            (should (equal hunk-notes-diff-id "open-test"))))
      (when-let ((buf (get-buffer name)))
        (kill-buffer buf)))))

(ert-deftest hunk-notes-toggle-and-leave-review ()
  (hunk-notes-test--with-fixture
   (lambda ()
     (goto-char (point-min))
     (search-forward "+  (message \"new\")")
     (hunk-notes-add-comment "Visible")
     (should hunk-notes-comments-visible)
     (hunk-notes-hide-comments)
     (should-not hunk-notes-comments-visible)
     (should (null hunk-notes--overlays))
     (hunk-notes-show-comments)
     (should hunk-notes-comments-visible)
     (should hunk-notes--overlays)
     (hunk-notes-leave-review)
     (should-not hunk-notes-mode)
     (should (null hunk-notes--overlays)))))

(ert-deftest hunk-notes-mode-line-lighter-shows-counts ()
  (hunk-notes-test--with-fixture
   (lambda ()
     (goto-char (point-min))
     (search-forward "+  (message \"new\")")
     (let ((comment (hunk-notes-add-comment "Count me")))
       (setf (hunk-notes-comment-resolved comment) t)
       (setq hunk-notes-comments-visible nil)
       (let ((lighter (substring-no-properties
                       (hunk-notes--mode-line-lighter))))
         (should (string-match-p "Hunk-Notes\\[1 hidden resolved:1\\]" lighter)))))))

;;; hunk-notes-tests.el ends here
