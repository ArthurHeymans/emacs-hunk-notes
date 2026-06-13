;;; ai-code-review.el --- Local inline code review comments for AI agents -*- lexical-binding: t; -*-

;; Author: ai-code-review contributors
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, vc, convenience

;;; Commentary:

;; Review any unified diff locally with inline comments, then generate or send a
;; structured prompt for an AI coding agent.

;;; Code:

(require 'diff-mode)
(require 'cl-lib)
(require 'subr-x)
(require 'vc)
(require 'ai-code-review-core)
(require 'ai-code-review-diff)
(require 'ai-code-review-comments)
(require 'ai-code-review-overlays)
(require 'ai-code-review-storage)
(require 'ai-code-review-prompt)
(require 'ai-code-review-agents)

(declare-function ai-code-review-comment-dwim "ai-code-review-comments" (&rest args))
(declare-function ai-code-review-comment-block-dwim "ai-code-review-comments" (&rest args))
(declare-function ai-code-review-remove-comment "ai-code-review-comments" (&rest args))

(defcustom ai-code-review-auto-enable-in-diff-buffers nil
  "When non-nil, automatically enable review mode in matching diff buffers.
The matching contexts are controlled by
`ai-code-review-auto-enable-contexts'."
  :type 'boolean
  :group 'ai-code-review)

(defcustom ai-code-review-auto-enable-contexts '(diff-mode magit vc jj)
  "Diff-buffer contexts where review mode may be auto-enabled.
This only has an effect when `ai-code-review-auto-enable-in-diff-buffers' is
non-nil.  Supported values are `diff-mode', `magit', `vc' and `jj'."
  :type '(set (const :tag "Ordinary diff-mode buffers" diff-mode)
              (const :tag "Magit diff buffers" magit)
              (const :tag "VC diff buffers" vc)
              (const :tag "Jujutsu compact diff buffers" jj))
  :group 'ai-code-review)

(defcustom ai-code-review-auto-enable-show-comments nil
  "When non-nil, show inline comments in auto-enabled diff buffers.
The default keeps auto-enabled buffers visually close to ordinary diffs until
the user toggles comments on."
  :type 'boolean
  :group 'ai-code-review)

(defcustom ai-code-review-auto-enable-render-style 'overlay
  "Buffer-local comment rendering style used for auto-enabled diff buffers.
Set this to nil to use `ai-code-review-render-style' unchanged."
  :type '(choice (const :tag "Use normal render style" nil)
                 (const :tag "Inserted read-only text blocks" inline-text)
                 (const :tag "Overlay virtual text" overlay))
  :group 'ai-code-review)

(defvar ai-code-review--suppress-auto-enable nil
  "Non-nil while explicit review setup should not trigger auto-enable hooks.")

(defvar-local ai-code-review--auto-enabled nil
  "Non-nil when the current review buffer was enabled automatically.")

(defvar-local ai-code-review--preserve-major-mode nil
  "Non-nil when review mode should not coerce the buffer to `diff-mode'.")

(defun ai-code-review--diff-like-mode-p ()
  "Return non-nil when the current major mode can host review overlays."
  (or (derived-mode-p 'diff-mode)
      (derived-mode-p 'magit-diff-mode 'magit-revision-mode)))

(defun ai-code-review--make-mode-map ()
  "Return the keymap for `ai-code-review-mode'."
  (let ((map (make-sparse-keymap)))
    ;; `C-c ,' is in the user/minor-mode-friendly punctuation space and avoids
    ;; common diff-mode bindings such as `C-c C-r'.
    (define-key map (kbd "C-c , c") #'ai-code-review-comment-dwim)
    (define-key map (kbd "C-c , C") #'ai-code-review-comment-block-dwim)
    (define-key map (kbd "C-c , x") #'ai-code-review-remove-comment)
    (define-key map (kbd "C-c , n") #'ai-code-review-next-comment)
    (define-key map (kbd "C-c , p") #'ai-code-review-previous-comment)
    (define-key map (kbd "C-c , l") #'ai-code-review-list-comments)
    (define-key map (kbd "C-c , t") #'ai-code-review-toggle-comments)
    (define-key map (kbd "C-c , r") #'ai-code-review-toggle-resolved)
    (define-key map (kbd "C-c , g") #'ai-code-review-show-prompt)
    (define-key map (kbd "C-c , y") #'ai-code-review-copy-prompt)
    (define-key map (kbd "C-c , s") #'ai-code-review-send-prompt-to-agent)
    (define-key map (kbd "C-c , q") #'ai-code-review-leave-review)
    (define-key map (kbd "C-c , ?") #'ai-code-review-dispatch)
    (define-key map (kbd "C-c , d") #'ai-code-review-dispatch)
    map))

(defvar ai-code-review-mode-map nil
  "Keymap for `ai-code-review-mode'.")

(setq ai-code-review-mode-map (ai-code-review--make-mode-map))

(defvar ai-code-review-mode-line-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] #'ai-code-review-toggle-comments)
    map)
  "Mode-line keymap for `ai-code-review-mode'.")

(defun ai-code-review--mode-line-lighter ()
  "Return a compact, clickable mode-line lighter for review state."
  (let* ((total (length ai-code-review-comments))
         (resolved (cl-count-if #'ai-code-review-comment-resolved
                                ai-code-review-comments))
         (parts (delq nil
                      (list (when (> total 0) (number-to-string total))
                            (when (not ai-code-review-comments-visible) "hidden")
                            (when (> resolved 0)
                              (format "resolved:%d" resolved)))))
         (text (concat " AI-Review"
                       (when parts
                         (format "[%s]" (string-join parts " "))))))
    (propertize text
                'help-echo "mouse-1: toggle review comments; C-c , ?: review menu"
                'mouse-face 'mode-line-highlight
                'local-map ai-code-review-mode-line-map)))

(defun ai-code-review--key-hint (command)
  "Return a concise key hint for COMMAND."
  (let ((key (where-is-internal command ai-code-review-mode-map t)))
    (if key (key-description key) "M-x")))

(defun ai-code-review--enable-message ()
  "Show a first-use hint for the current review buffer."
  (let ((count (length ai-code-review-comments)))
    (message "%s. %s comment, %s hide/show, %s copy prompt%s"
             (cond
              ((and (not ai-code-review-comments-visible) (> count 0))
               (format "AI review enabled with %d comment%s hidden"
                       count (if (= count 1) "" "s")))
              ((not ai-code-review-comments-visible)
               "AI review enabled with comments hidden")
              (t "AI review enabled"))
             (ai-code-review--key-hint #'ai-code-review-comment-dwim)
             (ai-code-review--key-hint #'ai-code-review-toggle-comments)
             (ai-code-review--key-hint #'ai-code-review-copy-prompt)
             (if ai-code-review--auto-enabled " (auto)" ""))))

(defun ai-code-review--dispatch-command (key)
  "Return the review command for dispatch KEY."
  (pcase (event-basic-type key)
    (?c #'ai-code-review-comment-dwim)
    (?C #'ai-code-review-comment-block-dwim)
    (?b #'ai-code-review-comment-block-dwim)
    (?x #'ai-code-review-remove-comment)
    (?n #'ai-code-review-next-comment)
    (?p #'ai-code-review-previous-comment)
    (?l #'ai-code-review-list-comments)
    (?t #'ai-code-review-toggle-comments)
    (?r #'ai-code-review-toggle-resolved)
    (?g #'ai-code-review-show-prompt)
    (?y #'ai-code-review-copy-prompt)
    (?s #'ai-code-review-send-prompt-to-agent)
    (?q #'ai-code-review-leave-review)
    (_ nil)))

;;;###autoload
(defun ai-code-review-dispatch ()
  "Prompt for a common AI review action and run it."
  (interactive)
  (let* ((key (read-key
               "AI review: c comment, C/b block, x remove, n/p next/prev, l list, t toggle, r resolve, g prompt, y copy, s send, q quit"))
         (command (ai-code-review--dispatch-command key)))
    (unless command
      (user-error "No AI review command bound to %s" (single-key-description key)))
    (call-interactively command)))

;;;###autoload
(define-minor-mode ai-code-review-mode
  "Inline AI code review comments for unified diff buffers.

Key bindings use the `C-c ,' prefix:
\{ai-code-review-mode-map}"
  :lighter (:eval (ai-code-review--mode-line-lighter))
  :keymap ai-code-review-mode-map
  (if ai-code-review-mode
      (progn
        (unless (or ai-code-review--preserve-major-mode
                    (ai-code-review--diff-like-mode-p))
          (let ((ai-code-review--suppress-auto-enable t))
            (diff-mode)))
        (unless (local-variable-p 'ai-code-review-comments-visible)
          (setq ai-code-review-comments-visible ai-code-review-show-comments-on-enable))
        (unless ai-code-review-comments
          (setq ai-code-review-comments nil))
        (ai-code-review-load-review)
        (ai-code-review-render-comments)
        (ai-code-review--enable-message))
    (ai-code-review-clear-overlays)))

(defun ai-code-review-start (&rest metadata)
  "Enable `ai-code-review-mode' for the current diff buffer with METADATA.
METADATA is a plist accepting :repo-root, :backend, :base-revision,
:target-revision, :diff-id, :comments-visible, :auto and
:preserve-major-mode."
  ;; `diff-mode' is a major mode and clears ordinary buffer-local variables, so
  ;; enter it before recording review metadata.
  (unless (or (plist-get metadata :preserve-major-mode)
              (ai-code-review--diff-like-mode-p))
    (let ((ai-code-review--suppress-auto-enable t))
      (diff-mode)))
  (setq ai-code-review-repo-root (or (plist-get metadata :repo-root)
                                     ai-code-review-repo-root
                                     default-directory)
        ai-code-review-backend (or (plist-get metadata :backend)
                                   ai-code-review-backend)
        ai-code-review-base-revision (or (plist-get metadata :base-revision)
                                         ai-code-review-base-revision)
        ai-code-review-target-revision (or (plist-get metadata :target-revision)
                                           ai-code-review-target-revision)
        ai-code-review-diff-id (or (plist-get metadata :diff-id)
                                   ai-code-review-diff-id)
        ai-code-review--auto-enabled (and (plist-get metadata :auto) t)
        ai-code-review--preserve-major-mode
        (and (plist-get metadata :preserve-major-mode) t))
  (when (plist-member metadata :comments-visible)
    (setq ai-code-review-comments-visible (plist-get metadata :comments-visible)))
  (unless (or (plist-member metadata :comments-visible)
              (plist-get metadata :auto))
    (setq ai-code-review-comments-visible ai-code-review-show-comments-on-enable))
  (ai-code-review-mode 1)
  (current-buffer))

(defun ai-code-review-open-diff-buffer (name diff-text &rest metadata)
  "Create or reuse diff buffer NAME containing DIFF-TEXT and start review.
METADATA is forwarded to `ai-code-review-start'."
  (let ((buf (get-buffer-create name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert diff-text)
        (goto-char (point-min)))
      (apply #'ai-code-review-start metadata))
    (pop-to-buffer buf)
    buf))

;;;###autoload
(defun ai-code-review-current-diff-buffer ()
  "Enable AI code review mode in the current unified diff buffer."
  (interactive)
  (ai-code-review-start :repo-root (or ai-code-review-repo-root default-directory)
                        :backend (or ai-code-review-backend 'diff)
                        :comments-visible ai-code-review-show-comments-on-enable))

;;;###autoload
(defun ai-code-review-leave-review ()
  "Disable AI review mode and return to an ordinary-looking diff buffer.
Comments remain in memory and, when storage is enabled, saved locally."
  (interactive)
  (unless ai-code-review-mode
    (user-error "AI code review mode is not enabled"))
  (ai-code-review-mode -1)
  (force-mode-line-update)
  (message "AI code review mode disabled; comments were preserved"))

(defun ai-code-review--buffer-looks-like-jj-diff-p ()
  "Return non-nil when the current diff resembles jj's compact format."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^\\(?:modified\\|added\\|removed\\) +" nil t)))

(defun ai-code-review--jj-root ()
  "Return the current jj repository root, or nil."
  (when (executable-find "jj")
    (with-temp-buffer
      (when (zerop (process-file "jj" nil t nil "root"))
        (string-trim (buffer-string))))))

(defun ai-code-review--magit-root ()
  "Return a Magit repository root when Magit helpers are available."
  (cond
   ((fboundp 'magit-toplevel) (magit-toplevel))
   ((fboundp 'magit-git-root) (magit-git-root))))

(defun ai-code-review--magit-revision ()
  "Return a Magit revision when available."
  (cond
   ((fboundp 'magit-commit-at-point) (magit-commit-at-point))
   ((boundp 'magit-buffer-revision) magit-buffer-revision)))

(defun ai-code-review--infer-auto-context ()
  "Infer the auto-enable context for the current diff buffer."
  (cond
   ((or (derived-mode-p 'magit-mode 'magit-diff-mode 'magit-revision-mode)
        (bound-and-true-p magit-buffer-revision))
    'magit)
   ((ai-code-review--buffer-looks-like-jj-diff-p) 'jj)
   ((string-match-p "\\`\\*vc.*diff" (downcase (buffer-name))) 'vc)
   ((derived-mode-p 'diff-mode) 'diff-mode)))

(defun ai-code-review--infer-auto-metadata (context)
  "Return review metadata inferred for auto-enable CONTEXT."
  (pcase context
    ('magit
     (list :repo-root (or (ignore-errors (ai-code-review--magit-root))
                          (ignore-errors (vc-root-dir))
                          default-directory)
           :backend 'magit
           :target-revision (ignore-errors (ai-code-review--magit-revision))
           :diff-id (format "magit:%s" (buffer-name))
           :preserve-major-mode t))
    ('jj
     (list :repo-root (or (ignore-errors (ai-code-review--jj-root))
                          default-directory)
           :backend 'jj
           :diff-id (format "jj:%s" (buffer-name))))
    ('vc
     (let ((root (ignore-errors (vc-root-dir)))
           (backend (ignore-errors (vc-responsible-backend default-directory))))
       (list :repo-root (or root default-directory)
             :backend (or backend 'vc)
             :diff-id (format "vc:%s" (buffer-name)))))
    (_
     (list :repo-root default-directory
           :backend 'diff
           :diff-id (format "diff:%s" (buffer-name))))))

(defun ai-code-review--maybe-auto-enable ()
  "Maybe enable `ai-code-review-mode' in the current diff buffer."
  (when (and ai-code-review-auto-enable-in-diff-buffers
             (not ai-code-review--suppress-auto-enable)
             (not ai-code-review-mode))
    (let ((context (ai-code-review--infer-auto-context)))
      (when (memq context ai-code-review-auto-enable-contexts)
        (when ai-code-review-auto-enable-render-style
          (setq-local ai-code-review-render-style
                      ai-code-review-auto-enable-render-style))
        (apply #'ai-code-review-start
               (append (ai-code-review--infer-auto-metadata context)
                       (list :comments-visible ai-code-review-auto-enable-show-comments
                             :auto t)))))))

(add-hook 'diff-mode-hook #'ai-code-review--maybe-auto-enable)

(with-eval-after-load 'magit
  (add-hook 'magit-diff-mode-hook #'ai-code-review--maybe-auto-enable)
  (add-hook 'magit-revision-mode-hook #'ai-code-review--maybe-auto-enable))

(provide 'ai-code-review)
;;; ai-code-review.el ends here
