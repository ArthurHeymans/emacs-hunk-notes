;;; hunk-notes-agents.el --- AI agent handoff for hunk-notes -*- lexical-binding: t; -*-

;;; Commentary:

;; Soft integrations for sending generated review prompts to AI agent frontends.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'hunk-notes-core)
(require 'hunk-notes-prompt)

(declare-function agent-shell-insert "agent-shell" (&rest args))
(declare-function eca-chat-send-prompt "eca" (prompt))
(declare-function gptel "gptel" (&optional name))
(declare-function gptel-send "gptel" (&rest args))
(declare-function pi-coding-agent "pi-coding-agent" (&rest args))
(declare-function pi "pi-coding-agent" (&rest args))
(declare-function ai-code-interface-send-prompt "ai-code-interface" (prompt))
(declare-function ai-code-send-prompt "ai-code-interface" (prompt))

(cl-defstruct (hunk-notes-agent
               (:constructor hunk-notes-agent-create))
  name
  available-p
  send-fn
  insert-fn
  description)

(defcustom hunk-notes-default-agent 'copy
  "Default target used by `hunk-notes-send-prompt-to-agent'."
  :type '(choice (const :tag "Copy to kill ring" copy)
                 (const :tag "Prompt buffer" buffer)
                 (const :tag "Shell command" shell)
                 (symbol :tag "Adapter symbol"))
  :group 'hunk-notes)

(defcustom hunk-notes-auto-submit-to-agent nil
  "When non-nil, adapters may submit prompts immediately.
The default is nil for safety because agents may edit files or run tools."
  :type 'boolean
  :group 'hunk-notes)

(defcustom hunk-notes-agent-shell-command nil
  "Shell command that receives the generated prompt on standard input."
  :type '(choice (const nil) string)
  :group 'hunk-notes)

(defcustom hunk-notes-agent-backends
  '(copy buffer shell agent-shell eca gptel gptel-agent pi ai-code)
  "Agent backend symbols offered by hunk-notes."
  :type '(repeat symbol)
  :group 'hunk-notes)

(defvar hunk-notes-before-prompt-functions nil
  "Hook run before generating a prompt for agent handoff.")

(defvar hunk-notes-after-prompt-functions nil
  "Hook run after generating a prompt for agent handoff.
Each function receives the prompt string.")

(defvar hunk-notes-before-send-functions nil
  "Hook run before sending a prompt to an agent.
Each function receives the agent symbol and prompt string.")

(defvar hunk-notes-after-send-hook nil
  "Hook run after sending a prompt to an agent.")

(defun hunk-notes--prompt-for-send ()
  "Generate a prompt for sending, running prompt hooks."
  (run-hooks 'hunk-notes-before-prompt-functions)
  (let ((prompt (hunk-notes-generate-prompt)))
    (run-hook-with-args 'hunk-notes-after-prompt-functions prompt)
    prompt))

(defun hunk-notes-send-prompt (&optional prompt)
  "Copy PROMPT to the kill ring, or generate one when PROMPT is nil."
  (interactive)
  (let ((prompt (or prompt (hunk-notes--prompt-for-send))))
    (kill-new prompt)
    (message "Copied hunk notes prompt (%d characters)" (length prompt))
    prompt))

(defun hunk-notes-send-to-buffer (&optional prompt)
  "Insert PROMPT into `*hunk-notes-prompt*' without submitting it anywhere."
  (interactive)
  (let ((prompt (or prompt (hunk-notes--prompt-for-send)))
        (buf (get-buffer-create "*hunk-notes-prompt*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert prompt)
      (goto-char (point-min)))
    (pop-to-buffer buf)))

(defun hunk-notes-send-to-shell-command (&optional prompt)
  "Send PROMPT to `hunk-notes-agent-shell-command' via standard input."
  (interactive)
  (unless (and hunk-notes-agent-shell-command
               (not (string-empty-p hunk-notes-agent-shell-command)))
    (user-error "Customize `hunk-notes-agent-shell-command' first"))
  (let ((prompt (or prompt (hunk-notes--prompt-for-send))))
    (with-temp-buffer
      (insert prompt)
      (shell-command-on-region (point-min) (point-max)
                               hunk-notes-agent-shell-command
                               "*hunk-notes-agent-shell*"))
    (message "Sent review prompt to shell command")))

(defun hunk-notes-send-to-eca (&optional prompt)
  "Send PROMPT to eca.el when available."
  (interactive)
  (unless (fboundp 'eca-chat-send-prompt)
    (require 'eca nil t))
  (unless (fboundp 'eca-chat-send-prompt)
    (user-error "ECA is not available"))
  (eca-chat-send-prompt (or prompt (hunk-notes--prompt-for-send))))

(defun hunk-notes-send-to-agent-shell (&optional prompt)
  "Insert PROMPT into agent-shell when available."
  (interactive)
  (unless (fboundp 'agent-shell-insert)
    (require 'agent-shell nil t))
  (unless (fboundp 'agent-shell-insert)
    (user-error "agent-shell is not available"))
  (agent-shell-insert :text (or prompt (hunk-notes--prompt-for-send))
                      :submit hunk-notes-auto-submit-to-agent))

(defun hunk-notes-send-to-gptel (&optional prompt)
  "Insert PROMPT into a gptel buffer when gptel is available."
  (interactive)
  (unless (fboundp 'gptel)
    (require 'gptel nil t))
  (unless (fboundp 'gptel)
    (user-error "gptel is not available"))
  (let ((prompt (or prompt (hunk-notes--prompt-for-send))))
    (gptel "*hunk-notes-gptel*")
    (goto-char (point-max))
    (insert prompt)
    (when (and hunk-notes-auto-submit-to-agent (fboundp 'gptel-send))
      (gptel-send))))

(defun hunk-notes-send-to-gptel-agent (&optional prompt)
  "Insert PROMPT for gptel-agent use when gptel is available."
  (interactive)
  (hunk-notes-send-to-gptel
   (concat "@gptel-agent\n\n" (or prompt (hunk-notes--prompt-for-send)))))

(defun hunk-notes-send-to-pi (&optional prompt)
  "Open a pi-coding-agent buffer and insert PROMPT when available."
  (interactive)
  (let ((prompt (or prompt (hunk-notes--prompt-for-send))))
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

(defun hunk-notes-send-to-ai-code (&optional prompt)
  "Send PROMPT through ai-code-interface.el when available."
  (interactive)
  (unless (featurep 'ai-code-interface)
    (require 'ai-code-interface nil t))
  (let ((prompt (or prompt (hunk-notes--prompt-for-send))))
    (cond
     ((fboundp 'ai-code-interface-send-prompt)
      (ai-code-interface-send-prompt prompt))
     ((fboundp 'ai-code-send-prompt)
      (ai-code-send-prompt prompt))
     (t
      (user-error "ai-code-interface.el is not available")))))

(defun hunk-notes-available-agents ()
  "Return a list of currently available agent symbols."
  (cl-remove-if-not
   (lambda (agent) (memq agent hunk-notes-agent-backends))
   (delq nil
         (list 'copy 'buffer
               (and hunk-notes-agent-shell-command 'shell)
               (and (or (fboundp 'agent-shell-insert)
                        (locate-library "agent-shell")) 'agent-shell)
               (and (or (fboundp 'eca-chat-send-prompt)
                        (locate-library "eca")) 'eca)
               (and (or (fboundp 'gptel) (locate-library "gptel")) 'gptel)
               (and (or (fboundp 'gptel) (locate-library "gptel")) 'gptel-agent)
               (and (or (fboundp 'pi-coding-agent) (fboundp 'pi)) 'pi)
               (and (or (featurep 'ai-code-interface)
                        (locate-library "ai-code-interface")) 'ai-code)))))

(defun hunk-notes-send-prompt-to-agent (agent)
  "Send generated prompt to AGENT.
Interactively, AGENT is read from available adapters."
  (interactive
   (list (intern (completing-read
                  "Send review prompt to: "
                  (mapcar #'symbol-name (hunk-notes-available-agents))
                  nil t nil nil (symbol-name hunk-notes-default-agent)))))
  (let ((prompt (hunk-notes--prompt-for-send)))
    (run-hook-with-args 'hunk-notes-before-send-functions agent prompt)
    (pcase agent
      ('copy (hunk-notes-send-prompt prompt))
      ('buffer (hunk-notes-send-to-buffer prompt))
      ('shell (hunk-notes-send-to-shell-command prompt))
      ('agent-shell (hunk-notes-send-to-agent-shell prompt))
      ('eca (hunk-notes-send-to-eca prompt))
      ('gptel (hunk-notes-send-to-gptel prompt))
      ('gptel-agent (hunk-notes-send-to-gptel-agent prompt))
      ('pi (hunk-notes-send-to-pi prompt))
      ('ai-code (hunk-notes-send-to-ai-code prompt))
      (_ (user-error "Unknown hunk-notes agent: %s" agent)))
    (run-hooks 'hunk-notes-after-send-hook)))

(provide 'hunk-notes-agents)
;;; hunk-notes-agents.el ends here
