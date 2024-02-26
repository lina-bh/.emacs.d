;; -*- lexical-binding: t; -*-
(eval-and-compile
  (require 'lina-package))

(use-package auto-compile
  :straight t
  :custom (auto-compile-on-load-mode t)
  :hook (emacs-lisp-mode . turn-on-auto-compile-mode))

(use-package exec-path-from-shell
  :straight t
  :if (eq system-type 'darwin)
  :custom (exec-path-from-shell-arguments '("-l"))
  :config (exec-path-from-shell-initialize))

(use-package diminish
  :straight t)
