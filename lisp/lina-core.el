;; -*- lexical-binding: t; -*-
(eval-when-compile
  (require 'use-package))
(require 'bind-key)

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
  :custom
  (auto-save-default nil) ;; TODO find some way of doing this without polluting
  (auto-insert-directory (locate-user-emacs-file "auto-insert/"))
  (blink-cursor-mode nil)
  (column-number-mode t)
  (create-lockfiles nil)
  (delete-selection-mode t)
  (eldoc-echo-area-use-multiline-p nil)
  (frame-title-format "%b")
  (indicate-empty-lines t)
  (inhibit-splash-screen t)
  (initial-scratch-message "")
  (make-backup-files nil)
  (mouse-wheel-follow-mouse 't)
  (mouse-wheel-progressive-speed nil)
  (mouse-wheel-scroll-amount '(1 ((shift) . 1)))
  (mouse-wheel-scroll-amount-horizontal 1)
  (mouse-wheel-tilt-scroll t)
  (native-comp-async-report-warnings-errors nil)
  (ring-bell-function 'ignore)
  (scroll-preserve-screen-position t)
  (scroll-step 1)
  (show-paren-mode nil)
  (tab-always-indent t)
  (use-dialog-box nil)
  (use-short-answers t)
  (vc-follow-symlinks t)
  (warning-minimum-level :error)
  :bind
  ("C-x 2" . #'split-and-follow-vertically)
  ("C-x 3" . #'split-and-follow-horizontally)
  ("C-k" . #'kill-whole-line)
  ("C-w" . #'c-w-dwim)
  ("M-i" . #'completion-at-point)
  (:map minibuffer-local-map
        ("C-u" . #'backward-kill-sentence))
  :diminish (eldoc-mode))

(use-package tab-line
  :if window-system
  :custom
  (tab-line-new-button-show nil)
  (global-tab-line-mode t))

(use-package xt-mouse
  :if (not window-system)
  :custom (xterm-mouse-mode t))

(use-package compile
  :defer t
  :config
  (setopt compilation-ask-about-save nil))

(use-package pixel-scroll
  :if window-system
  :custom (pixel-scroll-precision-mode t))

(use-package autorevert
  :custom (global-auto-revert-mode t)
  :diminish auto-revert-mode)

(use-package modus-themes
  :demand t
  :load-path
  (lambda ()
    (expand-file-name (concat data-directory "/themes")))
  :custom
  (modus-themes-fringes nil)
  (modus-themes-mode-line '(3d))
  (modus-themes-variable-pitch-ui t)
  (modus-themes-mixed-fonts t)
  (modus-themes-prompts '(intense))
  (modus-themes-subtle-line-numbers t)
  (modus-themes-syntax '(green-strings))
  (modus-themes-inhibit-reload nil)
  :custom-face
  (mode-line ((t (:inherit nil))))
  (mode-line-inactive ((t (:inherit nil))))
  (tab-line-tab ((t (:weight normal))))
  (tab-line-active ((t (:weight normal))))
  (tab-line-highlight ((t (:inherit nil))))
  :config
  (modus-themes-load-themes)
  (modus-themes-load-operandi))

;; backup-directory-alist (let ((backup-directory (locate-user-emacs-file "backups/")))
;;                          (make-directory backup-directory t)
;;                          (list
;;                           (cons "." backup-directory)))
;; tls
;; gnutls-min-prime-bits 3072
;; network-security-level 'medium
