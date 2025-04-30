(use-package shell
  :ensure nil
  :custom
  (async-shell-command-buffer 'new-buffer)
  (shell-command-prompt-show-cwd t)
  :hook (shell-command-mode . view-mode))
