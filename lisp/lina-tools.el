;; -*- lexical-binding: t; -*-
(use-package magit
  :ensure)
;; :custom
;; (magit-auto-revert-mode nil)
;; (global-auto-revert-mode t)
;; :diminish auto-revert-mode
;; :bind
;; ("C-c g c" . #'magit-clone)
;; ("C-c g d" . #'magit-diff-dwim)
(use-package magit-autorevert
  :custom (magit-auto-revert-mode nil))

(use-package vterm
  :ensure
  :custom ((vterm-always-compile-module t)
           (vterm-timer-delay 0.001)
	   (vterm-kill-buffer-on-exit nil)))

(use-package dired
  :custom (delete-by-moving-to-trash t)
  :hook (dired-mode . dired-hide-details-mode))
(use-package dired-single
  :ensure
  :after dired
  :bind
  ("C-x C-d" . dired)
  (:map dired-mode-map
        ([remap dired-find-file] . dired-single-buffer)
	([remap dired-mouse-find-file-other-window] . dired-single-buffer-mouse)
	([remap dired-up-directory] . dired-single-up-directory)))
