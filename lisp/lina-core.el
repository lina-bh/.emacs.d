;; -*- lexical-binding: t; -*-
(use-package emacs
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
  (package-native-compile t)
  (ring-bell-function 'ignore)
  (scroll-preserve-screen-position t)
  (scroll-step 1)
  (show-paren-mode nil)
  (tab-always-indent t)
  (use-dialog-box nil)
  (use-short-answers t)
  (vc-follow-symlinks t)
  (warning-minimum-level :error)
  :diminish eldoc-mode
  )
;; (set-face-attribute 'fringe nil :background (face-background 'default))

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

(use-package modus-themes
  :load-path
  (lambda ()
    (expand-file-name (concat lisp-directory "../etc/themes")))
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
  (tab-line-tab-current ((t (:weight normal))))
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
