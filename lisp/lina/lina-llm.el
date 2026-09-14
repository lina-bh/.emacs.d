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
  :pin melpa
  :custom
  ((gptel-log-level 'info))
  :init
  (when (fboundp 'markdown-ts-mode)
    (setopt gptel-default-mode 'markdown-ts-mode))
  :config
  (defun lina-gptel-hook ()
    (keymap-local-set "C-c C-c" #'gptel-send))
  (gptel-make-gemini "Gemini"
    :host "ai.lamancha-alhena.ts.net"
    :models '(gemini-3.8-flash)
    :stream t
    :key "dummy")
  :hook ((gptel-mode-hook . lina-gptel-hook)
         (gptel-post-stream-hook . gptel-auto-scroll)
         (gptel-post-response-functions . gptel-end-of-response)))

(use-package agent-shell
  :custom
  ((agent-shell-anthropic-claude-acp-command
    '("npx" "-y" "@agentclientprotocol/claude-agent-acp"))
   (agent-shell-pi-acp-command '("npx" "-y" "pi-acp"))
   (agent-shell-header-style 'text)
   (agent-shell-preferred-agent-config 'claude-code)
   (agent-shell-display-action nil)
   (agent-shell-transcript-file-path-function nil)
   (agent-shell-context-sources nil)
   (agent-shell-file-completion-enabled nil)
   (agent-shell-buffer-name-format
    (lambda (agent project)
      (format "*agent-shell %s @ %s*" agent project)))
   (agent-shell-highlight-blocks nil)
   (agent-shell-session-choices-function nil)
   (agent-shell-show-welcome-message nil)
   (agent-shell-thought-process-expand-by-default t)
   (shell-maker-prompt-before-killing-buffer nil))
  :config
  (defun lina-agent-shell-hook ()
    (keymap-local-set "C-a" #'beginning-of-line)
    (keymap-local-set "C-e" #'end-of-line))
  :hook (agent-shell-mode-hook . lina-agent-shell-hook))

(provide 'lina-llm)
;;; lina-llm.el ends here
