;; -*- lexical-binding: t; -*-
(use-package shell
  :ensure nil
  :custom
  (async-shell-command-buffer 'new-buffer)
  (shell-command-prompt-show-cwd t)
  (explicit-shell-file-name "zsh")
  :hook (shell-command-mode . view-mode))
