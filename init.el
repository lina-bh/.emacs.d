;; -*- lexical-binding: t; -*-
;(profiler-start 'cpu)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'load-path (locate-user-emacs-file "site-lisp/use-package/"))
(eval-when-compile
  (require 'use-package))
(require 'bind-key)

(use-package diminish
  :ensure t)

(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file t)

(add-to-list 'load-path (locate-user-emacs-file "lisp/"))

(require 'lina-vars)
(require 'lina-keys)
(require 'lina-faces)
(require 'lina-spell)
(require 'lina-org)
(require 'lina-dired)
(require 'lina-lsp)
(require 'lina-python)
(require 'lina-compl)
(require 'lina-modes)

(when (eq system-type 'windows-nt)
  (require 'lina-w32))
(when (eq system-type 'darwin)
  (require 'lina-macos))

;(profiler-stop)

