;; -*- lexical-binding: t; -*-
(use-package gptel
  :pin nongnu
  :custom
  (gptel-backend nil)
  (gptel-log-level 'debug)
  (gptel-directives '((default . "\
Assume the following:
* Respond as a computer program to which language is the interface.
* Do not respond conversationally, but concisely.
* Suggest methods and symbols, and sparingly example code blocks.
* Do not rewrite files and return them.")))
  :config
  (defun lina/gptel-hook ()
    (local-set-key (kbd "C-c C-c") #'gptel-send))
  (ignore-error 'user-error
    (gptel-make-openai "OpenRouter"
      :host "openrouter.ai"
      :endpoint "/api/v1/chat/completions"
      :stream t
      :key (gptel-api-key-from-auth-source "openrouter.ai")
      :models '(openrouter/auto))
    (gptel-make-gemini "Gemini"
      :key (gptel-api-key-from-auth-source
            "generativelanguage.googleapis.com")
      :stream t))
  :hook (gptel-mode-hook . lina/gptel-hook))

(use-package agent-shell
  :custom
  ((agent-shell-anthropic-claude-acp-command
    '("npx" "@agentclientprotocol/claude-agent-acp"))
   (agent-shell-header-style 'text)
   (agent-shell-preferred-agent-config 'claude-code)
   (agent-shell-display-action nil)
   (agent-shell-transcript-file-path-function nil)
   (agent-shell-context-sources nil)
   (agent-shell-file-completion-enabled nil)
   (agent-shell-buffer-name-format
    (lambda (agent project)
      (format "*agent-shell %s @ %s*" agent project)))
   (agent-shell-anthropic-default-model-id "opus")
   (shell-maker-prompt-before-killing-buffer nil)))

(provide 'lina-llm)
