(use-package nix-mode
  :ensure t
  :config
  (defun my-nix-hook ()
    (add-hook 'before-save-hook #'nix-format-before-save nil 'local))
  :hook (nix-mode . my-nix-hook))
