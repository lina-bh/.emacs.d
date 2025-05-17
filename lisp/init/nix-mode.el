;; -*- lexical-binding: t; -*-
(require 'mode-local)

(use-package nix-mode
  :ensure t
  :config
  (defun my-nix-hook ()
    (add-hook 'before-save-hook #'nix-format-before-save nil 'local))
  (defun lina-nix-repl-hook ()
    (when (fboundp 'corfu-mode)
      (corfu-mode -1)))
  :hook
  (nix-mode . my-nix-hook)
  (nix-repl-mode . lina-nix-repl-hook))
