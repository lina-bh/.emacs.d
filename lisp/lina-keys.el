;; -*- lexical-binding: t; -*-
(eval-when-compile
  (require 'use-package)
  (require 'bind-key))

(use-package emacs
  :init
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
  :bind
  ("C-x 2" . #'split-and-follow-vertically)
  ("C-x 3" . #'split-and-follow-horizontally)
  ("C-k" . #'kill-whole-line)
  ("C-w" . #'c-w-dwim)
  ("M-i" . #'completion-at-point)
  (:map minibuffer-local-map
        ("C-u" . #'backward-kill-sentence)))

(use-package which-key
  :ensure
  :custom (which-key-mode t)
  :diminish which-key-mode)
