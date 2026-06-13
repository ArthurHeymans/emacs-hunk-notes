;;; ai-code-review-agents.el --- AI agent handoff for ai-code-review -*- lexical-binding: t; -*-

;;; Commentary:

;; Soft integrations for sending generated review prompts to AI agent frontends.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'ai-code-review-core)
(require 'ai-code-review-prompt)

(declare-function agent-shell-insert "agent-shell" (&rest args))
(declare-function eca-chat-send-prompt "eca" (prompt))
(declare-function gptel "gptel" (&optional name))
(declare-function gptel-send "gptel" (&rest args))
(declare-function pi-coding-agent "pi-coding-agent" (&rest args))
(declare-function pi "pi-coding-agent" (&rest args))
(declare-function ai-code-interface-send-prompt "ai-code-interface" (prompt))
(declare-function ai-code-send-prompt "ai-code-interface" (prompt))

(cl-defstruct (ai-code-review-agent
               (:constructor ai-code-review-agent-create))
  name
  available-p
  send-fn
  insert-fn
  description)

(defcustom ai-code-review-default-agent 'copy
  "Default target used by `ai-code-review-send-prompt-to-agent'."
  :type '(choice (const :tag "Copy to kill ring" copy)
                 (const :tag "Prompt buffer" buffer)
                 (const :tag "Shell command" shell)
                 (symbol :tag "Adapter symbol"))
  :group 'ai-code-review)

(defcustom ai-code-review-auto-submit-to-agent nil
  "When non-nil, adapters may submit prompts immediately.
The default is nil for safety because agents may edit files or run tools."
  :type 'boolean
  :group 'ai-code-review)

(defcustom ai-code-review-agent-shell-command nil
  "Shell command that receives the generated prompt on standard input."
  :type '(choice (const nil) string)
  :group 'ai-code-review)

(defcustom ai-code-review-agent-backends
  '(copy buffer shell agent-shell eca gptel gptel-agent pi ai-code)
  "Agent backend symbols offered by ai-code-review."
  :type '(repeat symbol)
  :group 'ai-code-review)

(defvar ai-code-review-before-prompt-functions nil
  "Hook run before generating a prompt for agent handoff.")

(defvar ai-code-review-after-prompt-functions nil
  "Hook run after generating a prompt for agent handoff.
Each function receives the prompt string.")

(defvar ai-code-review-before-send-functions nil
  "Hook run before sending a prompt to an agent.
Each function receives the agent symbol and prompt string.")

(defvar ai-code-review-after-send-hook nil
  "Hook run after sending a prompt to an agent.")

(defun ai-code-review--prompt-for-send ()
  "Generate a prompt for sending, running prompt hooks."
  (run-hooks 'ai-code-review-before-prompt-functions)
  (let ((prompt (ai-code-review-generate-prompt)))
    (run-hook-with-args 'ai-code-review-after-prompt-functions prompt)
    prompt))

(defun ai-code-review-send-prompt (&optional prompt)
  "Copy PROMPT to the kill ring, or generate one when PROMPT is nil."
  (interactive)
  (let ((prompt (or prompt (ai-code-review--prompt-for-send))))
    (kill-new prompt)
    (message "Copied AI code review prompt (%d characters)" (length prompt))
    prompt))

(defun ai-code-review-send-to-buffer (&optional prompt)
  "Insert PROMPT into `*ai-code-review-prompt*' without submitting it anywhere."
  (interactive)
  (let ((prompt (or prompt (ai-code-review--prompt-for-send)))
        (buf (get-buffer-create "*ai-code-review-prompt*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert prompt)
      (goto-char (point-min)))
    (pop-to-buffer buf)))

(defun ai-code-review-send-to-shell-command (&optional prompt)
  "Send PROMPT to `ai-code-review-agent-shell-command' via standard input."
  (interactive)
  (unless (and ai-code-review-agent-shell-command
               (not (string-empty-p ai-code-review-agent-shell-command)))
    (user-error "Customize `ai-code-review-agent-shell-command' first"))
  (let ((prompt (or prompt (ai-code-review--prompt-for-send))))
    (with-temp-buffer
      (insert prompt)
      (shell-command-on-region (point-min) (point-max)
                               ai-code-review-agent-shell-command
                               "*ai-code-review-agent-shell*"))
    (message "Sent review prompt to shell command")))

(defun ai-code-review-send-to-eca (&optional prompt)
  "Send PROMPT to eca.el when available."
  (interactive)
  (unless (fboundp 'eca-chat-send-prompt)
    (require 'eca nil t))
  (unless (fboundp 'eca-chat-send-prompt)
    (user-error "ECA is not available"))
  (eca-chat-send-prompt (or prompt (ai-code-review--prompt-for-send))))

(defun ai-code-review-send-to-agent-shell (&optional prompt)
  "Insert PROMPT into agent-shell when available."
  (interactive)
  (unless (fboundp 'agent-shell-insert)
    (require 'agent-shell nil t))
  (unless (fboundp 'agent-shell-insert)
    (user-error "agent-shell is not available"))
  (agent-shell-insert :text (or prompt (ai-code-review--prompt-for-send))
                      :submit ai-code-review-auto-submit-to-agent))

(defun ai-code-review-send-to-gptel (&optional prompt)
  "Insert PROMPT into a gptel buffer when gptel is available."
  (interactive)
  (unless (fboundp 'gptel)
    (require 'gptel nil t))
  (unless (fboundp 'gptel)
    (user-error "gptel is not available"))
  (let ((prompt (or prompt (ai-code-review--prompt-for-send))))
    (gptel "*ai-code-review-gptel*")
    (goto-char (point-max))
    (insert prompt)
    (when (and ai-code-review-auto-submit-to-agent (fboundp 'gptel-send))
      (gptel-send))))

(defun ai-code-review-send-to-gptel-agent (&optional prompt)
  "Insert PROMPT for gptel-agent use when gptel is available."
  (interactive)
  (ai-code-review-send-to-gptel
   (concat "@gptel-agent\n\n" (or prompt (ai-code-review--prompt-for-send)))))

(defun ai-code-review-send-to-pi (&optional prompt)
  "Open a pi-coding-agent buffer and insert PROMPT when available."
  (interactive)
  (let ((prompt (or prompt (ai-code-review--prompt-for-send))))
    (cond
     ((fboundp 'pi-coding-agent)
      (pi-coding-agent)
      (goto-char (point-max))
      (insert prompt))
     ((fboundp 'pi)
      (pi)
      (goto-char (point-max))
      (insert prompt))
     (t
      (user-error "pi-coding-agent is not available")))))

(defun ai-code-review-send-to-ai-code (&optional prompt)
  "Send PROMPT through ai-code-interface.el when available."
  (interactive)
  (unless (featurep 'ai-code-interface)
    (require 'ai-code-interface nil t))
  (let ((prompt (or prompt (ai-code-review--prompt-for-send))))
    (cond
     ((fboundp 'ai-code-interface-send-prompt)
      (ai-code-interface-send-prompt prompt))
     ((fboundp 'ai-code-send-prompt)
      (ai-code-send-prompt prompt))
     (t
      (user-error "ai-code-interface.el is not available")))))

(defun ai-code-review-available-agents ()
  "Return a list of currently available agent symbols."
  (cl-remove-if-not
   (lambda (agent) (memq agent ai-code-review-agent-backends))
   (delq nil
         (list 'copy 'buffer
               (and ai-code-review-agent-shell-command 'shell)
               (and (or (fboundp 'agent-shell-insert)
                        (locate-library "agent-shell")) 'agent-shell)
               (and (or (fboundp 'eca-chat-send-prompt)
                        (locate-library "eca")) 'eca)
               (and (or (fboundp 'gptel) (locate-library "gptel")) 'gptel)
               (and (or (fboundp 'gptel) (locate-library "gptel")) 'gptel-agent)
               (and (or (fboundp 'pi-coding-agent) (fboundp 'pi)) 'pi)
               (and (or (featurep 'ai-code-interface)
                        (locate-library "ai-code-interface")) 'ai-code)))))

(defun ai-code-review-send-prompt-to-agent (agent)
  "Send generated prompt to AGENT.
Interactively, AGENT is read from available adapters."
  (interactive
   (list (intern (completing-read
                  "Send review prompt to: "
                  (mapcar #'symbol-name (ai-code-review-available-agents))
                  nil t nil nil (symbol-name ai-code-review-default-agent)))))
  (let ((prompt (ai-code-review--prompt-for-send)))
    (run-hook-with-args 'ai-code-review-before-send-functions agent prompt)
    (pcase agent
      ('copy (ai-code-review-send-prompt prompt))
      ('buffer (ai-code-review-send-to-buffer prompt))
      ('shell (ai-code-review-send-to-shell-command prompt))
      ('agent-shell (ai-code-review-send-to-agent-shell prompt))
      ('eca (ai-code-review-send-to-eca prompt))
      ('gptel (ai-code-review-send-to-gptel prompt))
      ('gptel-agent (ai-code-review-send-to-gptel-agent prompt))
      ('pi (ai-code-review-send-to-pi prompt))
      ('ai-code (ai-code-review-send-to-ai-code prompt))
      (_ (user-error "Unknown ai-code-review agent: %s" agent)))
    (run-hooks 'ai-code-review-after-send-hook)))

(provide 'ai-code-review-agents)
;;; ai-code-review-agents.el ends here
