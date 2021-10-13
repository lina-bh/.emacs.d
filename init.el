;; -*- lexical-binding: t; -*-

(add-to-list 'load-path "~/.emacs.d/use-package")
(require 'use-package)
(require 'bind-key)

;; (when (require 'benchmark-init nil t)
;;   (add-hook 'after-init-hook #'benchmark-init/deactivate))

(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file t)

(tool-bar-mode 0)
(show-paren-mode t)
(auto-save-visited-mode t)
(global-tab-line-mode t)
(global-eldoc-mode 0)
(desktop-save-mode t)

(setq-default
 mouse-wheel-scroll-amount '(1 ((shift) . 1))
 mouse-wheel-progressive-speed nil
 mouse-wheel-follow-mouse 't
 scroll-step 1
 scroll-preserve-screen-position t
 create-lockfiles nil
 make-backup-files nil
 inhibit-splash-screen t
 vc-follow-symlinks t
 frame-title-format "%b"
 w32-enable-synthesized-fonts t
 load-prefer-newer t
 gc-cons-threshold 1600000
 display-fill-column-indicator-column 80
 display-fill-column-indicator-character ?\s
 initial-frame-alist '(
		       (width . 90)
		       )
 default-frame-alist initial-frame-alist
 )
(defalias 'yes-or-no-p 'y-or-n-p)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)

(set-face-attribute 'fill-column-indicator nil
		    :background "red")
(set-face-attribute 'fringe nil :background (face-background 'default))

(load "functions.el")

(add-hook 'prog-mode-hook (defun my-prog-mode-hook ()
			    (column-number-mode t)
			    (display-line-numbers-mode t)
			    (electric-pair-local-mode t)
			    (setq-local indicate-empty-lines t
					show-trailing-whitespace t)))

(add-hook 'text-mode-hook (defun my-text-mode-hook ()
			    (visual-line-mode t)
			    (variable-pitch-mode t)
			    (setq-local cursor-type 'bar)))

(defun lina/elisp-save-hook ()
  (let ((user-emacs-directory (expand-file-name user-emacs-directory))
	(dir (file-name-directory buffer-file-name))
	(inhibit-message t))
    (when (or (string-equal user-emacs-directory dir)
	      (string-equal (expand-file-name "lisp/" user-emacs-directory)
			 dir))
      (byte-compile-file buffer-file-name))))
(add-hook 'emacs-lisp-mode-hook
	  (defun lina/elisp-hook ()
	    (add-hook 'kill-buffer-hook #'lina/elisp-save-hook nil t)
	    (display-fill-column-indicator-mode t)))

(defun split-and-follow-vertically ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(bind-key "C-x 2" #'split-and-follow-vertically)

(defun split-and-follow-horizontally ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(bind-key "C-x 3" #'split-and-follow-horizontally)

(bind-key "C-k" #'kill-whole-line)
(bind-key "C-w" #'backward-kill-word)

(bind-key "C-u" #'backward-kill-sentence minibuffer-local-map)

(require 'recentf)
(recentf-mode t)
(bind-key "C-x C-r" #'recentf-open-files)

(bind-keys :map emacs-lisp-mode-map
	   ("C-c f" . indent-buffer)
	   ("C-c e f" . indent-last-sexp))

(when (eq system-type 'gnu/linux)
  (set-face-attribute 'default nil :family "DejaVu Sans Mono"
		      :height 110)
  (set-face-attribute 'variable-pitch nil :family "Noto Sans")
  (load "js.el"))

(when (eq system-type 'windows-nt)
  (cd (getenv "USERPROFILE"))
  (set-face-attribute 'default nil
		      :family "Consolas"
		      :height 110)
  (set-face-attribute 'variable-pitch nil
		      :family "Calibri"
		      :height 115))

(use-package cua-base
  :init (cua-mode t)
  :bind (:map cua-global-keymap
	      ("C-z" . undo)
	      ("C-y" . nil)
	      ("C-s" . save-buffer)
	      ("C-f" . isearch-forward)
	      ("<C-return" . nil)))

(use-package undo-tree
  :ensure t
  :config (global-undo-tree-mode t)
  :bind (:map cua-global-keymap
	      ("C-z" . undo-tree-undo)
	      ("C-y" . undo-tree-redo)))

(use-package org
  :defer t
  :init (setq org-export-backends '(html odt)
	      org-modules nil)
  :config (setq-default org-adapt-indentation nil
			org-descriptive-links nil))

(use-package org-wc
  :load-path "~/.emacs.d/org-wc"
  :after org
  :bind (:map org-mode-map
	      ("C-c w" . org-wc-display)))

(use-package flycheck
  :disabled t
  :defer t
  :hook ((prog-mode . flycheck-mode)
	 (flycheck-error-list-mode . visual-line-mode))
  :config (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))


