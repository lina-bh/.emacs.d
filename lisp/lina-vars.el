;; -*- lexical-binding: t; -*-
(use-package emacs
  :custom
  (create-lockfiles nil)
  (display-fill-column-indicator-character ?\s)
  (display-fill-column-indicator-column 80)
  (frame-title-format "%b")
  (gc-cons-threshold 1600000)
  (inhibit-splash-screen t)
  (ring-bell-function 'ignore)
  (scroll-preserve-screen-position t)
  (scroll-step 1)
  (use-short-answers t)
  (warning-minimum-level :error)
  :init (delete 'Git vc-handled-backends))
;; initial-frame-alist '((width . 90))
;; default-frame-alist initial-frame-alist

(use-package mwheel
  :custom
  ((mouse-wheel-scroll-amount '(1 ((shift) . 1)))
   (mouse-wheel-progressive-speed nil)
   (mouse-wheel-follow-mouse 't)))
(use-package files
  :custom ((make-backup-files nil))
  :init (auto-save-visited-mode t))
(use-package vc-hooks
  :custom ((vc-follow-symlinks t)))
(use-package gnutls
  :custom ((gnutls-min-prime-bits 3072)))
(use-package nsm
  :custom ((network-security-level 'medium)))
(use-package comp
  :custom ((native-comp-async-report-warnings-errors nil)))
(use-package package
  :custom ((package-native-compile t)))

(setq-default mode-line-format
      '("%e" mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position
       (vc-mode vc-mode)
       "  " mode-line-modes mode-line-misc-info mode-line-end-spaces)
      mode-line-percent-position nil)

(provide 'lina-vars)
