;;; ai-code-review-tests.el --- Tests for ai-code-review -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path (file-name-directory (directory-file-name
                                             (file-name-directory load-file-name))))
(require 'ai-code-review)
(require 'ai-code-review-git)
(require 'ai-code-review-jj)

(defconst ai-code-review-test-fixture
  (expand-file-name "fixtures/simple.diff" (file-name-directory load-file-name)))

(defun ai-code-review-test--with-fixture (fn)
  (with-temp-buffer
    (insert-file-contents ai-code-review-test-fixture)
    (diff-mode)
    (let ((ai-code-review-auto-save nil)
          (ai-code-review--id-counter 0))
      (ai-code-review-start :repo-root "/tmp/repo" :backend 'git
                            :target-revision "working-copy"
                            :diff-id "test")
      (funcall fn))))

(defun ai-code-review-test--string-count (needle string)
  "Return number of non-overlapping NEEDLE matches in STRING."
  (let ((count 0)
        (start 0))
    (while (string-match (regexp-quote needle) string start)
      (setq start (match-end 0)
            count (1+ count)))
    count))

(ert-deftest ai-code-review-diff-parse-hunk-header ()
  (should (equal (ai-code-review-diff-parse-hunk-header "@@ -10,7 +10,9 @@")
                 '(:old-start 10 :old-count 7 :new-start 10 :new-count 9
                   :prefix-width 1)))
  (should (equal (ai-code-review-diff-parse-hunk-header "@@ -1 +2 @@")
                 '(:old-start 1 :old-count 1 :new-start 2 :new-count 1
                   :prefix-width 1)))
  (let ((combined (ai-code-review-diff-parse-hunk-header
                   "@@@ -1,3 -1,4 +1,5 @@@")))
    (should (plist-get combined :combined))
    (should (= (plist-get combined :new-start) 1))
    (should (= (plist-get combined :prefix-width) 2))))

(ert-deftest ai-code-review-diff-position-added-line ()
  (ai-code-review-test--with-fixture
   (lambda ()
     (goto-char (point-min))
     (search-forward "+  (message \"new\")")
     (let ((info (ai-code-review-diff-position-info)))
       (should (equal (plist-get info :file) "src/foo.el"))
       (should (eq (plist-get info :side) 'new))
       (should (= (plist-get info :line) 2))
       (should (eq (plist-get info :change-type) 'added))))))

(ert-deftest ai-code-review-diff-position-removed-line ()
  (ai-code-review-test--with-fixture
   (lambda ()
     (goto-char (point-min))
     (search-forward "-  (message \"old\")")
     (let ((info (ai-code-review-diff-position-info)))
       (should (equal (plist-get info :file) "src/foo.el"))
       (should (eq (plist-get info :side) 'old))
       (should (= (plist-get info :line) 2))
       (should (eq (plist-get info :change-type) 'removed))))))

(ert-deftest ai-code-review-diff-ignores-non-hunk-plus-lines ()
  (with-temp-buffer
    (insert "diff --git a/foo b/foo\n"
            "+not a real changed line yet\n"
            "@@ this is not a valid hunk header @@\n"
            "+still not in a parsed hunk\n")
    (diff-mode)
    (goto-char (point-min))
    (search-forward "+still")
    (should-not (ai-code-review-diff-position-info))))

(ert-deftest ai-code-review-diff-position-combined-added-line ()
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
    (let ((info (ai-code-review-diff-position-info)))
      (should (equal (plist-get info :file) "src/foo.el"))
      (should (eq (plist-get info :side) 'new))
      (should (= (plist-get info :line) 2))
      (should (eq (plist-get info :change-type) 'added)))))

(ert-deftest ai-code-review-diff-position-jj-compact-file-header ()
  (with-temp-buffer
    (insert "modified    README.org\n"
            "@@ -0,1 +1,3 @@\n"
            "+one\n"
            "+two\n")
    (diff-mode)
    (goto-char (point-min))
    (search-forward "+two")
    (let ((info (ai-code-review-diff-position-info)))
      (should (equal (plist-get info :file) "README.org"))
      (should (eq (plist-get info :side) 'new))
      (should (= (plist-get info :line) 2))
      (should (eq (plist-get info :change-type) 'added)))))

(ert-deftest ai-code-review-comments-add-edit-delete ()
  (ai-code-review-test--with-fixture
   (lambda ()
     (goto-char (point-min))
     (search-forward "+  (message \"new\")")
     (let ((comment (ai-code-review-add-comment "Please rename this message.")))
       (should (= (length ai-code-review-comments) 1))
       (should (equal (ai-code-review-comment-id comment) "R1"))
       (ai-code-review-edit-comment comment "Updated body")
       (should (equal (ai-code-review-comment-body comment) "Updated body"))
       (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
         (ai-code-review-delete-comment comment))
       (should (null ai-code-review-comments))))))

(ert-deftest ai-code-review-empty-edit-removes-comment ()
  (ai-code-review-test--with-fixture
   (lambda ()
     (goto-char (point-min))
     (search-forward "+  (message \"new\")")
     (let ((comment (ai-code-review-add-comment "Temporary")))
       (should (= (length ai-code-review-comments) 1))
       (ai-code-review-edit-comment comment "   ")
       (should (null ai-code-review-comments))))))

(ert-deftest ai-code-review-renumbers-comments-in-diff-order ()
  (ai-code-review-test--with-fixture
   (lambda ()
     (let ((ai-code-review-render-style 'overlay))
       (goto-char (point-min))
       (search-forward "+;; added")
       (let ((lower (ai-code-review-add-comment "Lower hunk")))
         (should (equal (ai-code-review-comment-id lower) "R1"))
         (goto-char (point-min))
         (search-forward "+  (message \"new\")")
         (let ((upper (ai-code-review-add-comment "Upper hunk")))
           (should (equal (ai-code-review-comment-id upper) "R1"))
           (should (equal (ai-code-review-comment-id lower) "R2"))
           (ai-code-review-remove-comment upper)
           (should (equal (ai-code-review-comment-id lower) "R1"))))))))

(ert-deftest ai-code-review-region-comment-line-range ()
  (ai-code-review-test--with-fixture
   (lambda ()
     (goto-char (point-min))
     (search-forward "+  (message \"new\")")
     (beginning-of-line)
     (push-mark (point) t t)
     (forward-line 2)
     (let ((comment (ai-code-review-comment-dwim "Two-line comment")))
       (should (= (ai-code-review-comment-line-start comment) 2))
       (should (= (ai-code-review-comment-line-end comment) 3))
       (should (string-match-p "lines 2-3"
                               (ai-code-review-generate-prompt)))))))

(ert-deftest ai-code-review-region-comment-allows-mixed-side-hunk-block ()
  (ai-code-review-test--with-fixture
   (lambda ()
     (goto-char (point-min))
     (search-forward "-  (message \"old\")")
     (beginning-of-line)
     (push-mark (point) t t)
     (forward-line 3)
     (let ((comment (ai-code-review-comment-dwim "Replacement block comment")))
       (should (eq (ai-code-review-comment-side comment) 'new))
       (should (= (ai-code-review-comment-old-line-start comment) 2))
       (should (= (ai-code-review-comment-old-line-end comment) 2))
       (should (= (ai-code-review-comment-new-line-start comment) 2))
       (should (= (ai-code-review-comment-new-line-end comment) 3))
       (let ((prompt (ai-code-review-generate-prompt)))
         (should (string-match-p "old line 2; new lines 2-3" prompt))
         (should (string-match-p "Replacement block comment" prompt)))))))

(ert-deftest ai-code-review-prompt-generation ()
  (ai-code-review-test--with-fixture
   (lambda ()
     (goto-char (point-min))
     (search-forward "+  (message \"extra\")")
     (ai-code-review-add-comment "This extra message should be justified.")
     (let ((prompt (ai-code-review-generate-prompt)))
       (should (string-match-p "Repository: /tmp/repo" prompt))
       (should (string-match-p "## Diff and review comments" prompt))
       (should (string-match-p "Comment R" prompt))
       (should (string-match-p "This extra message" prompt))))))

(ert-deftest ai-code-review-prompt-places-comments-next-to-hunks ()
  (ai-code-review-test--with-fixture
   (lambda ()
     (goto-char (point-min))
     (search-forward "+  (message \"extra\")")
     (ai-code-review-add-comment "Comment near first hunk")
     (let* ((prompt (ai-code-review-generate-prompt))
            (hunk-pos (string-match-p (regexp-quote "@@ -1,5 +1,6 @@")
                                      prompt))
            (comment-pos (string-match-p "Comment R1 on new line 3" prompt)))
       (should hunk-pos)
       (should comment-pos)
       (should (< hunk-pos comment-pos))
       (should (string-match-p "Review comments for this hunk" prompt))
       (should-not (string-match-p "## Review comments\n\n### File" prompt))))))

(ert-deftest ai-code-review-prompt-does-not-include-earlier-hunks-for-same-file ()
  (ai-code-review-test--with-fixture
   (lambda ()
     (goto-char (point-min))
     (search-forward "+;; added")
     (ai-code-review-add-comment "Second hunk only")
     (let ((prompt (ai-code-review-generate-prompt)))
       (should (string-match-p (regexp-quote "@@ -20,3 +21,4 @@") prompt))
       (should-not (string-match-p (regexp-quote "@@ -1,5 +1,6 @@") prompt))
       (should-not (string-match-p (regexp-quote "+  (message \"new\")")
                                   prompt))))))

(ert-deftest ai-code-review-prompt-groups-multiple-commented-hunks-by-file ()
  (ai-code-review-test--with-fixture
   (lambda ()
     (goto-char (point-min))
     (search-forward "+  (message \"extra\")")
     (ai-code-review-add-comment "First hunk comment")
     (goto-char (point-min))
     (search-forward "+;; added")
     (ai-code-review-add-comment "Second hunk comment")
     (let ((prompt (ai-code-review-generate-prompt)))
       (should (= (ai-code-review-test--string-count "### File: src/foo.el"
                                                     prompt)
                  1))
       (should (string-match-p (regexp-quote "@@ -1,5 +1,6 @@") prompt))
       (should (string-match-p (regexp-quote "@@ -20,3 +21,4 @@") prompt))
       (should (string-match-p "First hunk comment" prompt))
       (should (string-match-p "Second hunk comment" prompt))
       (should (= (ai-code-review-test--string-count
                   "Review comments for this hunk" prompt)
                  2))))))

(ert-deftest ai-code-review-storage-roundtrip ()
  (ai-code-review-test--with-fixture
   (lambda ()
     (let ((ai-code-review-storage-directory
            (make-temp-file "ai-code-review-tests" t)))
       (goto-char (point-min))
       (search-forward "+  (message \"new\")")
       (ai-code-review-add-comment "Persist me")
       (let ((file (ai-code-review-save-review)))
         (setq ai-code-review-comments nil)
         (ai-code-review-load-review file)
         (should (= (length ai-code-review-comments) 1))
         (should (equal (ai-code-review-comment-body (car ai-code-review-comments))
                        "Persist me")))))))

(ert-deftest ai-code-review-command-construction ()
  (should (equal (ai-code-review-git-working-copy-args)
                 '("diff" "--no-ext-diff" "--")))
  (should (equal (ai-code-review-git-range-args "main" "HEAD")
                 '("diff" "--no-ext-diff" "main" "HEAD" "--")))
  (should (equal (ai-code-review-jj-range-args "main" "@")
                 '("diff" "--from" "main" "--to" "@"))))

(ert-deftest ai-code-review-auto-enable-hides-comments-and-uses-overlay-style ()
  (let ((ai-code-review-auto-enable-in-diff-buffers t)
        (ai-code-review-auto-enable-show-comments nil)
        (ai-code-review-auto-enable-render-style 'overlay)
        (ai-code-review-auto-save nil))
    (with-temp-buffer
      (insert-file-contents ai-code-review-test-fixture)
      (diff-mode)
      (should ai-code-review-mode)
      (should ai-code-review--auto-enabled)
      (should-not ai-code-review-comments-visible)
      (should (eq ai-code-review-render-style 'overlay)))))

(ert-deftest ai-code-review-open-diff-buffer-preserves-start-metadata ()
  (let ((ai-code-review-auto-save nil)
        (name "*ai-code-review-test-open-diff-buffer*"))
    (unwind-protect
        (let ((buf (ai-code-review-open-diff-buffer
                    name
                    (with-temp-buffer
                      (insert-file-contents ai-code-review-test-fixture)
                      (buffer-string))
                    :repo-root "/tmp/repo"
                    :backend 'git
                    :target-revision "working-copy"
                    :diff-id "open-test"
                    :comments-visible nil)))
          (with-current-buffer buf
            (should (eq major-mode 'diff-mode))
            (should ai-code-review-mode)
            (should-not ai-code-review-comments-visible)
            (should (equal ai-code-review-repo-root "/tmp/repo"))
            (should (eq ai-code-review-backend 'git))
            (should (equal ai-code-review-target-revision "working-copy"))
            (should (equal ai-code-review-diff-id "open-test"))))
      (when-let ((buf (get-buffer name)))
        (kill-buffer buf)))))

(ert-deftest ai-code-review-toggle-and-leave-review ()
  (ai-code-review-test--with-fixture
   (lambda ()
     (goto-char (point-min))
     (search-forward "+  (message \"new\")")
     (ai-code-review-add-comment "Visible")
     (should ai-code-review-comments-visible)
     (ai-code-review-hide-comments)
     (should-not ai-code-review-comments-visible)
     (should (null ai-code-review--overlays))
     (ai-code-review-show-comments)
     (should ai-code-review-comments-visible)
     (should ai-code-review--overlays)
     (ai-code-review-leave-review)
     (should-not ai-code-review-mode)
     (should (null ai-code-review--overlays)))))

(ert-deftest ai-code-review-mode-line-lighter-shows-counts ()
  (ai-code-review-test--with-fixture
   (lambda ()
     (goto-char (point-min))
     (search-forward "+  (message \"new\")")
     (let ((comment (ai-code-review-add-comment "Count me")))
       (setf (ai-code-review-comment-resolved comment) t)
       (setq ai-code-review-comments-visible nil)
       (let ((lighter (substring-no-properties
                       (ai-code-review--mode-line-lighter))))
         (should (string-match-p "AI-Review\\[1 hidden resolved:1\\]" lighter)))))))

;;; ai-code-review-tests.el ends here
