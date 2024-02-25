;; -*- lexical-binding: t; -*-
(eval-and-compile
  (require 'lina-package))

(use-package magit
  :straight (:source melpa)
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

(use-package ess-site
  :defer t
  :straight ess)
