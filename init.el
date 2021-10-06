;; -*- lexical-binding: t; -*-

(show-paren-mode t)
(auto-save-visited-mode t)

(setq-default mouse-wheel-scroll-amount '(1 ((shift) . 1))
	      mouse-wheel-progressive-speed nil
	      mouse-wheel-follow-mouse 't
	      scroll-step 1
	      create-lockfiles nil
	      make-backup-files nil
	      inhibit-splash-screen t)

(require 'recentf)
(recentf-mode t)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)
(add-hook 'after-init-hook (defun my-after-init-hook ()
			     (recentf-open-files)))

(add-hook 'prog-mode-hook (defun my-prog-mode-hook ()
			    (display-line-numbers-mode t)
			    (electric-pair-local-mode t)))

(add-hook 'text-mode-hook (defun my-text-mode-hook ()
			    (visual-line-mode t)
			    (variable-pitch-mode t)
			    (setq-local cursor-style 'bar)))

(defun split-and-follow-horizontally ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)

(defun split-and-follow-vertically ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)

(global-set-key (kbd "C-k") #'kill-whole-line)

(require 'cua-base)
(cua-mode t)
(define-key cua-global-keymap (kbd "C-z") #'undo)
(define-key cua-global-keymap (kbd "C-y") nil)
(define-key cua-global-keymap (kbd "C-s") #'save-buffer)
(define-key cua-global-keymap (kbd "C-f") #'isearch-forward)
(define-key cua-global-keymap (kbd "C-w") #'backward-kill-word)

(defun indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max))))

(set-face-attribute 'fringe nil :background (face-background 'default))

(when (eq system-type 'gnu/linux)
  (tool-bar-mode 0)
  (set-face-attribute 'default nil :family "DejaVu Sans Mono"
		      :height 120)
  (set-face-attribute 'variable-pitch nil :family "Noto Sans"))

(when (eq system-type 'windows-nt)
  (cd (getenv "USERPROFILE"))
  (set-face-attribute 'default nil
		      :family "Consolas"
		      :height 110)
  (set-face-attribute 'variable-pitch nil
		      :family "Tahoma"
		      :height 110))

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(eval-when-compile
  (add-to-list 'load-path "~/.emacs.d/use-package")
  (require 'use-package))

(use-package undo-tree
  :ensure t
  :config (global-undo-tree-mode t)
  :bind (:map cua-global-keymap
	      ("C-z" . undo-tree-undo)
	      ("C-y" . undo-tree-redo)))

(use-package org
  :config (setq-default org-adapt-indentation nil
			org-descriptive-links nil))

(use-package flycheck
  :defer t
  :hook ((prog-mode . flycheck-mode))
  :config (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(use-package rjsx-mode
  :mode "\\.[m]?js\\'"
  :config (setq-default
	   js-chain-indent t
	   js-indent-level 2
	   js2-basic-offset 2))

(use-package add-node-modules-path
  :defer t
  :hook (((js2-mode rjsx-mode) . add-node-modules-path)))

(use-package prettier-js
  :defer t
  :hook (((js2-mode rjsx-mode) . prettier-js-mode)))
