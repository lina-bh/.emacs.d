;; -*- lexical-binding: t; -*-

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(setq-default mouse-wheel-scroll-amount '(1 ((shift) . 1))
	      mouse-wheel-progressive-speed nil
	      mouse-wheel-follow-mouse 't
	      scroll-step 1

	      create-lockfiles nil
	      make-backup-files nil

	      org-adapt-indentation nil
	      org-descriptive-links nil

	      inhibit-splash-screen t)

(add-hook 'prog-mode-hook (defun my-prog-mode-hook ()
			    (electric-pair-local-mode t)))

(add-hook 'text-mode-hook (defun my-text-mode-hook ()
			    (visual-line-mode t)
			    (variable-pitch-mode t)))

(add-hook 'after-init-hook (defun my-after-init-hook ()
			     (recentf-open-files)))

(ido-mode t)
(show-paren-mode t)
(auto-save-visited-mode t)
(global-undo-tree-mode t)

(recentf-mode t)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

(defun indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max))))

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

(cua-mode t)
(define-key cua-global-keymap (kbd "C-z") #'undo-tree-undo)
(define-key cua-global-keymap (kbd "C-y") #'undo-tree-redo)
(define-key cua-global-keymap (kbd "C-s") #'save-buffer)
(define-key cua-global-keymap (kbd "C-f") #'isearch-forward)

(set-face-attribute 'fringe nil :background nil)
(when (eq system-type 'gnu/linux)
  (tool-bar-mode 0)
  (set-face-attribute 'default nil :family "DejaVu Sans Mono"
		      :height 120)
  (set-face-attribute 'variable-pitch nil :family "Noto Sans"))
(when (eq system-type 'windows-nt)
  (cd (getenv "USERPROFILE")))
