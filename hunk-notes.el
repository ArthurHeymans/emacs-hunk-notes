;;; hunk-notes.el --- Local inline hunk notes for AI agents -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author: hunk-notes contributors
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
(require 'transient nil t)
(require 'hunk-notes-core)
(require 'hunk-notes-diff)
(require 'hunk-notes-comments)
(require 'hunk-notes-overlays)
(require 'hunk-notes-storage)
(require 'hunk-notes-prompt)
(require 'hunk-notes-agents)

(declare-function hunk-notes-comment-dwim "hunk-notes-comments" (&rest args))
(declare-function hunk-notes-comment-block-dwim "hunk-notes-comments" (&rest args))
(declare-function hunk-notes-remove-comment "hunk-notes-comments" (&rest args))
(declare-function hunk-notes-hide-comments "hunk-notes-overlays" ())
(declare-function hunk-notes-show-comments "hunk-notes-overlays" ())

(defcustom hunk-notes-auto-enable-in-diff-buffers nil
  "When non-nil, automatically enable review mode in matching diff buffers.
The matching contexts are controlled by
`hunk-notes-auto-enable-contexts'."
  :type 'boolean
  :group 'hunk-notes)

(defcustom hunk-notes-auto-enable-contexts '(diff-mode magit vc jj)
  "Diff-buffer contexts where review mode may be auto-enabled.
This only has an effect when `hunk-notes-auto-enable-in-diff-buffers' is
non-nil.  Supported values are `diff-mode', `magit', `vc' and `jj'."
  :type '(set (const :tag "Ordinary diff-mode buffers" diff-mode)
              (const :tag "Magit diff buffers" magit)
              (const :tag "VC diff buffers" vc)
              (const :tag "Jujutsu compact diff buffers" jj))
  :group 'hunk-notes)

(defcustom hunk-notes-auto-enable-show-comments nil
  "When non-nil, show inline comments in auto-enabled diff buffers.
The default keeps auto-enabled buffers visually close to ordinary diffs until
the user toggles comments on."
  :type 'boolean
  :group 'hunk-notes)

(defcustom hunk-notes-auto-enable-render-style 'overlay
  "Buffer-local comment rendering style used for auto-enabled diff buffers.
Set this to nil to use `hunk-notes-render-style' unchanged."
  :type '(choice (const :tag "Use normal render style" nil)
                 (const :tag "Inserted read-only text blocks" inline-text)
                 (const :tag "Overlay virtual text" overlay))
  :group 'hunk-notes)

(defvar hunk-notes--suppress-auto-enable nil
  "Non-nil while explicit review setup should not trigger auto-enable hooks.")

(defvar-local hunk-notes--auto-enabled nil
  "Non-nil when the current review buffer was enabled automatically.")

(defvar-local hunk-notes--preserve-major-mode nil
  "Non-nil when review mode should not coerce the buffer to `diff-mode'.")

(defun hunk-notes--diff-like-mode-p ()
  "Return non-nil when the current major mode can host review overlays."
  (or (derived-mode-p 'diff-mode)
      (derived-mode-p 'magit-diff-mode 'magit-revision-mode)))

(defun hunk-notes--make-mode-map ()
  "Return the keymap for `hunk-notes-mode'."
  (let ((map (make-sparse-keymap)))
    ;; `C-c ,' is in the user/minor-mode-friendly punctuation space and avoids
    ;; common diff-mode bindings such as `C-c C-r'.
    (define-key map (kbd "C-c , c") #'hunk-notes-comment-dwim)
    (define-key map (kbd "C-c , C") #'hunk-notes-comment-block-dwim)
    (define-key map (kbd "C-c , x") #'hunk-notes-remove-comment)
    (define-key map (kbd "C-c , n") #'hunk-notes-next-comment)
    (define-key map (kbd "C-c , p") #'hunk-notes-previous-comment)
    (define-key map (kbd "C-c , l") #'hunk-notes-list-comments)
    (define-key map (kbd "C-c , t") #'hunk-notes-toggle-comments)
    (define-key map (kbd "C-c , r") #'hunk-notes-toggle-resolved)
    (define-key map (kbd "C-c , g") #'hunk-notes-show-prompt)
    (define-key map (kbd "C-c , y") #'hunk-notes-copy-prompt)
    (define-key map (kbd "C-c , s") #'hunk-notes-send-prompt-to-agent)
    (define-key map (kbd "C-c , q") #'hunk-notes-leave-review)
    (define-key map (kbd "C-c , ?") #'hunk-notes-dispatch)
    (define-key map (kbd "C-c , d") #'hunk-notes-dispatch)
    map))

(defvar hunk-notes-mode-map nil
  "Keymap for `hunk-notes-mode'.")

(setq hunk-notes-mode-map (hunk-notes--make-mode-map))

(defvar hunk-notes-mode-line-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] #'hunk-notes-toggle-comments)
    map)
  "Mode-line keymap for `hunk-notes-mode'.")

(defun hunk-notes--mode-line-lighter ()
  "Return a compact, clickable mode-line lighter for review state."
  (let* ((total (length hunk-notes-comments))
         (resolved (cl-count-if #'hunk-notes-comment-resolved
                                hunk-notes-comments))
         (parts (delq nil
                      (list (when (> total 0) (number-to-string total))
                            (when (not hunk-notes-comments-visible) "hidden")
                            (when (> resolved 0)
                              (format "resolved:%d" resolved)))))
         (text (concat " Hunk-Notes"
                       (when parts
                         (format "[%s]" (string-join parts " "))))))
    (propertize text
                'help-echo "mouse-1: toggle review comments; C-c , ?: review menu"
                'mouse-face 'mode-line-highlight
                'local-map hunk-notes-mode-line-map)))

(defun hunk-notes--key-hint (command)
  "Return a concise key hint for COMMAND."
  (let ((key (where-is-internal command hunk-notes-mode-map t)))
    (if key (key-description key) "M-x")))

(defun hunk-notes--enable-message ()
  "Show a first-use hint for the current review buffer."
  (let ((count (length hunk-notes-comments)))
    (message "%s. %s comment, %s hide/show, %s copy prompt%s"
             (cond
              ((and (not hunk-notes-comments-visible) (> count 0))
               (format "Hunk notes enabled with %d comment%s hidden"
                       count (if (= count 1) "" "s")))
              ((not hunk-notes-comments-visible)
               "Hunk notes enabled with comments hidden")
              (t "Hunk notes enabled"))
             (hunk-notes--key-hint #'hunk-notes-comment-dwim)
             (hunk-notes--key-hint #'hunk-notes-toggle-comments)
             (hunk-notes--key-hint #'hunk-notes-copy-prompt)
             (if hunk-notes--auto-enabled " (auto)" ""))))

(defun hunk-notes--dispatch-command (key)
  "Return the review command for dispatch KEY."
  (pcase (event-basic-type key)
    (?c #'hunk-notes-comment-dwim)
    (?C #'hunk-notes-comment-block-dwim)
    (?b #'hunk-notes-comment-block-dwim)
    (?x #'hunk-notes-remove-comment)
    (?n #'hunk-notes-next-comment)
    (?p #'hunk-notes-previous-comment)
    (?l #'hunk-notes-list-comments)
    (?t #'hunk-notes-toggle-comments)
    (?h #'hunk-notes-hide-comments)
    (?S #'hunk-notes-show-comments)
    (?r #'hunk-notes-toggle-resolved)
    (?g #'hunk-notes-show-prompt)
    (?y #'hunk-notes-copy-prompt)
    (?s #'hunk-notes-send-prompt-to-agent)
    (?q #'hunk-notes-leave-review)
    (_ nil)))

;;;###autoload
(defun hunk-notes--read-key-dispatch ()
  "Prompt for a common Hunk notes action with a single key and run it."
  (interactive)
  (let* ((key (read-key
               "Hunk notes: c comment, C/b block, x remove, n/p next/prev, l list, t toggle, h hide, S show, r resolve, g prompt, y copy, s send, q quit"))
         (command (hunk-notes--dispatch-command key)))
    (unless command
      (user-error "No Hunk notes command bound to %s" (single-key-description key)))
    (call-interactively command)))

(when (featurep 'transient)
  (transient-define-prefix hunk-notes-transient ()
    "Transient menu for hunk notes commands."
    [["Comments"
      ("c" "add/edit" hunk-notes-comment-dwim)
      ("C" "add/edit block" hunk-notes-comment-block-dwim)
      ("x" "remove" hunk-notes-remove-comment)
      ("r" "resolve/reopen" hunk-notes-toggle-resolved)]
     ["Navigation"
      ("n" "next" hunk-notes-next-comment)
      ("p" "previous" hunk-notes-previous-comment)
      ("l" "list" hunk-notes-list-comments)]
     ["Display"
      ("t" "toggle comments" hunk-notes-toggle-comments)
      ("h" "hide comments" hunk-notes-hide-comments)
      ("S" "show comments" hunk-notes-show-comments)
      ("q" "leave review" hunk-notes-leave-review)]
     ["Prompt / Agent"
      ("g" "show prompt" hunk-notes-show-prompt)
      ("y" "copy prompt" hunk-notes-copy-prompt)
      ("s" "send to agent" hunk-notes-send-prompt-to-agent)]]))

;;;###autoload
(defun hunk-notes-dispatch ()
  "Open the hunk notes dispatcher.
Use a Transient menu when `transient' is available; otherwise fall back to a
single-key prompt."
  (interactive)
  (if (fboundp 'hunk-notes-transient)
      (hunk-notes-transient)
    (hunk-notes--read-key-dispatch)))

;;;###autoload
(define-minor-mode hunk-notes-mode
  "Inline hunk notes comments for unified diff buffers.

Key bindings use the `C-c ,' prefix:
\{hunk-notes-mode-map}"
  :lighter (:eval (hunk-notes--mode-line-lighter))
  :keymap hunk-notes-mode-map
  (if hunk-notes-mode
      (progn
        (unless (or hunk-notes--preserve-major-mode
                    (hunk-notes--diff-like-mode-p))
          (let ((hunk-notes--suppress-auto-enable t))
            (diff-mode)))
        (unless (local-variable-p 'hunk-notes-comments-visible)
          (setq hunk-notes-comments-visible hunk-notes-show-comments-on-enable))
        (unless hunk-notes-comments
          (setq hunk-notes-comments nil))
        (hunk-notes-load-review)
        (hunk-notes-render-comments)
        (hunk-notes--enable-message))
    (hunk-notes-clear-overlays)))

(defun hunk-notes-start (&rest metadata)
  "Enable `hunk-notes-mode' for the current diff buffer with METADATA.
METADATA is a plist accepting :repo-root, :backend, :base-revision,
:target-revision, :diff-id, :comments-visible, :auto and
:preserve-major-mode."
  ;; `diff-mode' is a major mode and clears ordinary buffer-local variables, so
  ;; enter it before recording review metadata.
  (unless (or (plist-get metadata :preserve-major-mode)
              (hunk-notes--diff-like-mode-p))
    (let ((hunk-notes--suppress-auto-enable t))
      (diff-mode)))
  (setq hunk-notes-repo-root (or (plist-get metadata :repo-root)
                                     hunk-notes-repo-root
                                     default-directory)
        hunk-notes-backend (or (plist-get metadata :backend)
                                   hunk-notes-backend)
        hunk-notes-base-revision (or (plist-get metadata :base-revision)
                                         hunk-notes-base-revision)
        hunk-notes-target-revision (or (plist-get metadata :target-revision)
                                           hunk-notes-target-revision)
        hunk-notes-diff-id (or (plist-get metadata :diff-id)
                                   hunk-notes-diff-id)
        hunk-notes--auto-enabled (and (plist-get metadata :auto) t)
        hunk-notes--preserve-major-mode
        (and (plist-get metadata :preserve-major-mode) t))
  (when (plist-member metadata :comments-visible)
    (setq hunk-notes-comments-visible (plist-get metadata :comments-visible)))
  (unless (or (plist-member metadata :comments-visible)
              (plist-get metadata :auto))
    (setq hunk-notes-comments-visible hunk-notes-show-comments-on-enable))
  (hunk-notes-mode 1)
  (current-buffer))

(defun hunk-notes-open-diff-buffer (name diff-text &rest metadata)
  "Create or reuse diff buffer NAME containing DIFF-TEXT and start review.
METADATA is forwarded to `hunk-notes-start'."
  (let ((buf (get-buffer-create name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert diff-text)
        (goto-char (point-min)))
      (apply #'hunk-notes-start metadata))
    (pop-to-buffer buf)
    buf))

;;;###autoload
(defun hunk-notes-current-diff-buffer ()
  "Enable hunk notes mode in the current unified diff buffer."
  (interactive)
  (hunk-notes-start :repo-root (or hunk-notes-repo-root default-directory)
                        :backend (or hunk-notes-backend 'diff)
                        :comments-visible hunk-notes-show-comments-on-enable))

;;;###autoload
(defun hunk-notes-leave-review ()
  "Disable Hunk notes mode and return to an ordinary-looking diff buffer.
Comments remain in memory and, when storage is enabled, saved locally."
  (interactive)
  (unless hunk-notes-mode
    (user-error "hunk notes mode is not enabled"))
  (hunk-notes-mode -1)
  (force-mode-line-update)
  (message "hunk notes mode disabled; comments were preserved"))

(defun hunk-notes--buffer-looks-like-jj-diff-p ()
  "Return non-nil when the current diff resembles jj's compact format."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^\\(?:modified\\|added\\|removed\\) +" nil t)))

(defun hunk-notes--jj-root ()
  "Return the current jj repository root, or nil."
  (when (executable-find "jj")
    (with-temp-buffer
      (when (zerop (process-file "jj" nil t nil "root"))
        (string-trim (buffer-string))))))

(defun hunk-notes--magit-root ()
  "Return a Magit repository root when Magit helpers are available."
  (cond
   ((fboundp 'magit-toplevel) (magit-toplevel))
   ((fboundp 'magit-git-root) (magit-git-root))))

(defun hunk-notes--magit-revision ()
  "Return a Magit revision when available."
  (cond
   ((fboundp 'magit-commit-at-point) (magit-commit-at-point))
   ((boundp 'magit-buffer-revision) magit-buffer-revision)))

(defun hunk-notes--infer-auto-context ()
  "Infer the auto-enable context for the current diff buffer."
  (cond
   ((or (derived-mode-p 'magit-mode 'magit-diff-mode 'magit-revision-mode)
        (bound-and-true-p magit-buffer-revision))
    'magit)
   ((hunk-notes--buffer-looks-like-jj-diff-p) 'jj)
   ((string-match-p "\\`\\*vc.*diff" (downcase (buffer-name))) 'vc)
   ((derived-mode-p 'diff-mode) 'diff-mode)))

(defun hunk-notes--infer-auto-metadata (context)
  "Return review metadata inferred for auto-enable CONTEXT."
  (pcase context
    ('magit
     (list :repo-root (or (ignore-errors (hunk-notes--magit-root))
                          (ignore-errors (vc-root-dir))
                          default-directory)
           :backend 'magit
           :target-revision (ignore-errors (hunk-notes--magit-revision))
           :diff-id (format "magit:%s" (buffer-name))
           :preserve-major-mode t))
    ('jj
     (list :repo-root (or (ignore-errors (hunk-notes--jj-root))
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

(defun hunk-notes--maybe-auto-enable ()
  "Maybe enable `hunk-notes-mode' in the current diff buffer."
  (when (and hunk-notes-auto-enable-in-diff-buffers
             (not hunk-notes--suppress-auto-enable)
             (not hunk-notes-mode))
    (let ((context (hunk-notes--infer-auto-context)))
      (when (memq context hunk-notes-auto-enable-contexts)
        (when hunk-notes-auto-enable-render-style
          (setq-local hunk-notes-render-style
                      hunk-notes-auto-enable-render-style))
        (apply #'hunk-notes-start
               (append (hunk-notes--infer-auto-metadata context)
                       (list :comments-visible hunk-notes-auto-enable-show-comments
                             :auto t)))))))

(add-hook 'diff-mode-hook #'hunk-notes--maybe-auto-enable)

(with-eval-after-load 'magit
  (add-hook 'magit-diff-mode-hook #'hunk-notes--maybe-auto-enable)
  (add-hook 'magit-revision-mode-hook #'hunk-notes--maybe-auto-enable))

(provide 'hunk-notes)
;;; hunk-notes.el ends here
