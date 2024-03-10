;; -*- lexical-binding: t; -*-
(use-package emacs
  :init
  (defun lina-set-vc-backends ()
    (setq vc-handled-backends '(Git)))

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
  (blink-cursor-mode nil)
  (column-number-mode t)
  (show-paren-mode nil)
  (save-place-mode t)
  (delete-selection-mode t)
  (global-goto-address-mode t)
  ;; (vc-follow-symlinks t)
  :config
  (setq
   auto-save-default nil
   create-lockfiles nil
   enable-recursive-minibuffers t
   frame-title-format "%b"
   indicate-empty-lines t
   inhibit-splash-screen t
   initial-major-mode #'fundamental-mode
   initial-scratch-message ""
   make-backup-files nil
   mouse-wheel-follow-mouse 't
   mouse-wheel-progressive-speed nil
   mouse-wheel-scroll-amount '(1 ((shift) . 1))
   mouse-wheel-scroll-amount-horizontal 1
   mouse-wheel-tilt-scroll t
   native-comp-async-report-warnings-errors nil
   ring-bell-function #'ignore
   scroll-preserve-screen-position t
   scroll-step 1
   show-paren-context-when-offscreen 'overlay
   tab-always-indent 'complete
   use-dialog-box nil
   use-short-answers t
   warning-minimum-level :error
   )
  :hook (after-init . lina-set-vc-backends)
  :bind
  ("C-x 2" . split-and-follow-vertically)
  ("C-x 3" . split-and-follow-horizontally)
  ("C-k" . kill-whole-line)
  ("C-w" . c-w-dwim)
  ("M-i" . completion-at-point)
  ("M-;" . comment-line)
  (:map minibuffer-local-map
        ("C-u" . backward-kill-sentence)))

(use-package eldoc
  :custom (eldoc-echo-area-use-multiline-p nil)
  :diminish eldoc-mode)

(use-package tab-line
  :if window-system
  :custom
  (tab-line-new-button-show nil)
  (global-tab-line-mode t))

(use-package xt-mouse
  :if (not window-system)
  :custom (xterm-mouse-mode t))

(use-package compile
  :config
  (setq-default compilation-ask-about-save nil
                compile-command "make "
                compilation-scroll-output 'first-error))

;; (use-package pixel-scroll
;;   :disabled t
;;   :if window-system
;;   :custom (pixel-scroll-precision-mode t))

(use-package autorevert
  :custom (global-auto-revert-mode t)
  :diminish auto-revert-mode)

(use-package modus-themes
  :if window-system
  :demand t
  :load-path
  (lambda ()
    (expand-file-name "./themes" data-directory))
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
  :config (load-theme 'modus-operandi t))

;; backup-directory-alist (let ((backup-directory (locate-user-emacs-file "backups/")))
;;                          (make-directory backup-directory t)
;;                          (list
;;                           (cons "." backup-directory)))
;; tls
;; gnutls-min-prime-bits 3072
;; network-security-level 'medium
