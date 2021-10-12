;; -*- lexical-binding: t; -*-

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file t)

(show-paren-mode t)
(auto-save-visited-mode t)
(global-tab-line-mode t)
(global-eldoc-mode 0)

(setq-default mouse-wheel-scroll-amount '(1 ((shift) . 1))
	      mouse-wheel-progressive-speed nil
	      mouse-wheel-follow-mouse 't
	      scroll-step 1
	      scroll-preserve-screen-position t
	      create-lockfiles nil
	      make-backup-files nil
	      inhibit-splash-screen t
	      vc-follow-symlinks t
	      frame-title-format "%b"
	      w32-enable-synthesized-fonts t)

(desktop-save-mode t)
(require 'recentf)
(recentf-mode t)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)
(add-hook 'desktop-not-loaded-hook (defun my-after-init-hook ()
				     (recentf-open-files)))

(add-hook 'prog-mode-hook (defun my-prog-mode-hook ()
			    (display-line-numbers-mode t)
			    (electric-pair-local-mode t)
			    (setq-local indicate-empty-lines t)))

(add-hook 'text-mode-hook (defun my-text-mode-hook ()
			    (visual-line-mode t)
			    (variable-pitch-mode t)
			    (setq-local cursor-type 'bar)))

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
(global-set-key (kbd "C-w") #'backward-kill-word)

(define-key minibuffer-local-map (kbd "C-u") #'backward-kill-sentence)



(defun indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max))))

(defalias 'yes-or-no-p 'y-or-n-p)

(set-face-attribute 'fringe nil :background (face-background 'default))

(when (eq system-type 'gnu/linux)
  (tool-bar-mode 0)
  (set-face-attribute 'default nil :family "DejaVu Sans Mono"
		      :height 110)
  (set-face-attribute 'variable-pitch nil :family "Noto Sans"))

(when (eq system-type 'windows-nt)
  (cd (getenv "USERPROFILE"))
  (set-face-attribute 'default nil
		      :family "Consolas"
		      :height 110)
  (set-face-attribute 'variable-pitch nil
		      :family "Calibri"
		      :height 115))

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(eval-when-compile
  (add-to-list 'load-path "~/.emacs.d/use-package")
  (require 'use-package))

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
  :config (setq-default org-adapt-indentation nil
			org-descriptive-links nil))

(use-package org-wc
  :config (fset #'org-wc-put-overlay #'my--org-wc-put-overlay)
  :bind (:map org-mode-map
	      ("C-c w" . org-wc-display)))

(defun my--org-wc-put-overlay (wc)
  (let* ((c 60)
         (off 0)
         ov tx)
    (org-move-to-column c)
    (unless (eolp) (skip-chars-backward "^ \t"))
    (skip-chars-backward " \t")
    (setq ov (make-overlay (1- (point)) (point-at-eol))
          tx (concat (buffer-substring (1- (point)) (point))
                     (make-string (+ off (max 0 (- c (current-column)))) ?.)
                     (org-add-props (format "%s" (number-to-string wc))
                         (list 'face 'org-wc-overlay))
                     ""))
    (overlay-put ov 'display tx)
    (push ov org-wc-overlays)))

(use-package flycheck
  :disabled t
  :defer t
  :hook ((prog-mode . flycheck-mode)
	 (flycheck-error-list-mode . visual-line-mode))
  :config (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(use-package rjsx-mode
  :mode "\\.[m]?js\\'"
  :config (setq-default
	   js-chain-indent t
	   js-indent-level 2
	   js2-basic-offset 2)
  :bind (:map rjsx-mode-map
	      ("<" . nil)
	      ("C-d" . nil)
	      (">" . nil)))

(use-package add-node-modules-path
  :defer t
  :hook (((js2-mode rjsx-mode) . add-node-modules-path)))

(use-package prettier-js
  :defer t
  :hook (((js2-mode rjsx-mode) . prettier-js-mode)))

(use-package eglot
  :defer t
  :config (setq-default eglot-stay-out-of '(flymake)))
