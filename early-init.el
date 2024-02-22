;; -*- lexical-binding: t; no-byte-compile: t; -*-
(remove-hook 'find-file-hook 'vc-find-file-hook)
(setq-default load-prefer-newer t
	      gc-cons-threshold most-positive-fixnum
	      vc-handled-backends nil)

(unless noninteractive
  (add-to-list 'load-path (locate-user-emacs-file "lisp/benchmark-init-el/"))
  (require 'benchmark-init-loaddefs)
  (benchmark-init/activate)
  (add-hook 'emacs-startup-hook #'benchmark-init/deactivate))

(tool-bar-mode 0) ;; prevent flash
(if (eq system-type 'darwin)
    (progn
      (add-to-list 'load-path
		   (locate-user-emacs-file "lisp/exec-path-from-shell"))
      (require 'exec-path-from-shell)
      (setq exec-path-from-shell-arguments '("-l"))
      (exec-path-from-shell-initialize))
  (menu-bar-mode 0))
