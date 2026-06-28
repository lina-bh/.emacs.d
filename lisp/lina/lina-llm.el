;; -*- lexical-binding: t; -*-
(use-package gptel
  :pin nongnu
  :custom
  (gptel-log-level 'debug)
  (gptel-model 'gemini-2.5-flash)
  (gptel-directives '((default . "\
Assume the following:
* Respond as a computer program to which language is the interface.
* Do not respond conversationally, but concisely.
* Suggest methods and symbols, and sparingly example code blocks.
* Do not rewrite files and return them.")))
  :config
  (defun lina/gptel-hook ()
    (local-set-key (kbd "C-c C-c") #'gptel-send))
  (setopt gptel-backend (gptel-make-gemini "Gemini"
                                           :key (gptel-api-key-from-auth-source "generativelanguage.googleapis.com")
                                           :stream t))

  :hook (gptel-mode-hook . lina/gptel-hook))

(use-package agent-shell
  :custom
  ((agent-shell-anthropic-claude-acp-command
    '("pnpx"
      "@agentclientprotocol/claude-agent-acp"))
   (agent-shell-header-style 'text)
   (agent-shell-preferred-agent-config 'claude-code)
   (agent-shell-display-action nil)
   (agent-shell-transcript-file-path-function nil)
   (agent-shell-context-sources nil)
   (agent-shell-thought-process-expand-by-default t)))

(provide 'lina-llm)
