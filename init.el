;; -*- lexical-binding: t; -*-
(eval-when-compile
  (require 'use-package)
  (require 'bind-key))

(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file t)

(use-package diminish
  :straight t)

(use-package exec-path-from-shell
  :if (eq system-type 'darwin)
  :straight t
  :commands exec-path-from-shell-initialize
  :custom (exec-path-from-shell-arguments '("-l"))
  :config (exec-path-from-shell-initialize))

(add-to-list 'load-path (locate-user-emacs-file "lisp/"))
(load "lina-core")
(load "lina-compl")
(load "lina-modes")
(load "lina-check")
(load "lina-keys")
(load "lina-tex")
(load "lina-org")
(load "lina-cmds")

(use-package magit
  :straight t
  :defer t
  :custom
  (magit-auto-revert-mode nil)
  (global-auto-revert-mode t)
  :diminish auto-revert-mode)

(use-package vterm
  :straight t
  :defer t
  :custom ((vterm-always-compile-module t)
           (vterm-timer-delay 0.001)
	   (vterm-kill-buffer-on-exit nil)))

(use-package dired
  :commands dired-get-file-for-visit
  :custom ((delete-by-moving-to-trash t))
  :init
  (defun lina-dired-shellopen-at-point ()
    (interactive)
    (start-process "dired-open" nil "open" (dired-get-file-for-visit)))
  :hook (dired-mode . dired-hide-details-mode)
  :bind (:map dired-mode-map
              ("E" . #'lina-dired-shellopen-at-point)))
(use-package dired-single
  :straight t
  :after (dired)
  :bind (:map dired-mode-map
	      ([remap dired-find-file]
	       .
	       dired-single-buffer)
	      ([remap dired-mouse-find-file-other-window]
	       .
	       dired-single-buffer-mouse)
	      ([remap dired-up-directory]
	       .
	       dired-single-up-directory)))

(use-package auto-compile
  :defer t
  :hook (emacs-lisp-mode . turn-on-auto-compile-mode))

(use-package ess-site
  :defer t
  :straight ess)

(pcase system-type
  ('windows-nt (load-library "lina-w32"))
  ('darwin (load-library "lina-macos")))
