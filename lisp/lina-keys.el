;; -*- lexical-binding: t; -*-
(eval-when-compile
  (require 'use-package)
  (require 'bind-key))

(defun split-and-follow-vertically ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))

(defun split-and-follow-horizontally ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))

(defun c-w-dwim (beg end)
  (interactive "r")
  (if (use-region-p)
      (kill-region beg end)
    (backward-kill-word 1)))

(bind-key "C-x 2" #'split-and-follow-vertically)
(bind-key "C-x 3" #'split-and-follow-horizontally)
(bind-key "C-k" #'kill-whole-line)
(bind-key "C-w" #'c-w-dwim)
(bind-key "C-u" #'backward-kill-sentence minibuffer-local-map)
(bind-key "C-x C-g" #'keyboard-quit)
(bind-key "C-c C-g" #'keyboard-quit)
(bind-key "M-i" #'completion-at-point)
(bind-key "C-h i" #'info-lookup-symbol)

(use-package ffap
  :bind (("C-x C-d" . #'dired-at-point)
         ("C-x C-f" . #'find-file-at-point)))

(use-package recentf
  :custom (recentf-mode t)
  :bind (("C-x C-r" . #'recentf-open)))

(use-package undo-tree
  :disabled t
  :ensure
  :diminish undo-tree-mode
  :custom ((undo-tree-auto-save-history nil)
	   (global-undo-tree-mode t)))

(use-package which-key
  :disabled t
  :diminish which-key-mode
  :custom (which-key-mode t))
