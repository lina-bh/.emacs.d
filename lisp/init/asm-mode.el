(use-package asm-mode
  :ensure nil
  :config
  (setq-mode-local asm-mode tab-width 2)
  (defun my-asm-mode-hook ()
    (when (boundp 'asm-comment-char)
      (local-unset-key (vector asm-comment-char)))
    (unbind-key ":" 'asm-mode-map))
  :hook (asm-mode . my-asm-mode-hook))
