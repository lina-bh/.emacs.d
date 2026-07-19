;;; lina-llm.el --- lina-llm  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Lina Bhaile <emacs-devel@linabee.uk>

;; Author: Lina Bhaile <emacs-devel@linabee.uk>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(autoload 'notifications-notify "notifications")
(autoload 'ghostel-send-string "ghostel")

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
  (ignore-error user-error
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
   (shell-maker-prompt-before-killing-buffer nil))
  :config
  (defun lina-agent-shell-hook ()
    (keymap-local-set "C-a" #'beginning-of-line)
    (keymap-local-set "C-e" #'end-of-line))
  :hook (agent-shell-mode-hook . lina-agent-shell-hook))

(use-package claude-code
  :autoload (claude-code-send-escape
             lina-claude-notify
             lina-claude-C-z)
  :vc (:url "https://github.com/stevemolitor/claude-code.el" :rev :newest)
  :custom ((claude-code-terminal-backend 'ghostel)
           (claude-code-display-window-fn #'display-buffer)
           (claude-code-program-switches
            (list "--settings" (json-encode '(:theme "light-ansi")))))
  :config
  (defun lina-claude-C-z ()
    (interactive)
    (ghostel-send-string (kbd "^_")))
  (defun lina-claude-term-hook ()
    (if (and (eq claude-code-terminal-backend 'ghostel)
             (eq major-mode 'ghostel-mode)
             (string-match-p "\\*claude" (buffer-name)))
        (progn
          (keymap-local-set "C-g" #'claude-code-send-escape)
          (keymap-local-set "C-z" #'lina-claude-C-z))
      (error "Wrong buffer for hook: %S %S" (buffer-name) major-mode)))
  (setopt claude-code-notification-function
          (eval `(defun lina-claude-notify (title message)
                   ,@(when (featurep 'dbusbind)
                       '((notifications-notify :title title
                                               :body message)))
                   (message "%s: %s" title message))
                t))
  :hook (claude-code-start-hook . lina-claude-term-hook))

(use-package monet
  :vc (:url "https://github.com/stevemolitor/monet" :rev :newest)
  :custom ((monet-diff-tool nil))
  :hook (claude-code-process-environment-functions
         .
         monet-start-server-function))

(use-package claude-code-ide
  :disabled t
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
  :custom ((claude-code-ide-terminal-backend 'ghostel)
           (claude-code-ide-cli-extra-flags (concat "--settings '"
                                                    (json-encode
                                                     '(:theme "light-ansi"))
                                                    "'"))
           (claude-code-ide-enable-execute-code nil)
           (claude-code-ide-focus-claude-after-ediff nil)
           (claude-code-ide-mcp-allowed-tools 'auto)
           (claude-code-ide-no-flicker nil)
           (claude-code-ide-show-claude-window-in-ediff nil)
           (claude-code-ide-switch-tab-on-ediff nil)
           (claude-code-ide-use-ide-diff nil)
           (claude-code-ide-use-side-window nil)
           (claude-code-ide-debug t)))

(provide 'lina-llm)
;;; lina-llm.el ends here
