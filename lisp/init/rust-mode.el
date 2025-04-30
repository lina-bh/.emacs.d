(use-package rust-mode
  :ensure t
  :defines rust-mode-map
  :custom
  (rust-mode-treesitter-derive t)
  (rust-format-on-save t)
  :init
  (setq-mode-local rust-mode format-mode-command #'eglot-format)
  :mode "\\.rs\\'"
  :bind (:map rust-mode-map
              ("C-c C-c" . rust-compile)
              ("C-c C-k" . rust-check)))
