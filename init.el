;; -*- lexical-binding: t; -*-

(add-to-list 'load-path (locate-user-emacs-file "lisp/"))

(package-initialize)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)

(add-to-list 'load-path (locate-user-emacs-file "site-lisp/use-package/"))
(require 'use-package)
(require 'bind-key)

(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file t)

(tool-bar-mode 0)
;; (show-paren-mode t)
(auto-save-visited-mode t)
(global-tab-line-mode t)
(global-eldoc-mode 0)
(desktop-save-mode t)

(require 'lina-vars)
(require 'lina-funs)
(require 'lina-hooks)
(require 'lina-keys)
(require 'lina-faces)
(require 'lina-spell)
(require 'lina-org)

(when (eq system-type 'windows-nt)
  (require 'lina-w32))
