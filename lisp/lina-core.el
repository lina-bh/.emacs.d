;; -*- lexical-binding: t; -*-
(eval-when-compile
  (require 'bind-key))

(setopt
 blink-cursor-mode nil
 column-number-mode t
 compilation-ask-about-save nil
 create-lockfiles nil
 delete-selection-mode t
 frame-title-format "%b"
 indicate-empty-lines t
 inhibit-splash-screen t
 native-comp-async-report-warnings-errors nil
 package-native-compile t
 ring-bell-function 'ignore
 scroll-preserve-screen-position t
 scroll-step 1
 tab-always-indent t
 use-dialog-box nil
 use-short-answers t
 vc-follow-symlinks t
 warning-minimum-level :error
 )
(set-face-attribute 'fringe nil :background (face-background 'default))
(when (display-graphic-p)
  (modify-all-frames-parameters '((height . 57)
				  (width . 99)
				  (cursor-type . bar)))
  (setf (alist-get 'top initial-frame-alist) 0)
  (setf (alist-get 'left initial-frame-alist) 0))

(use-package display-line-numbers
  :custom
  (display-line-numbers-grow-only t)
  (display-line-numbers-width-start 3)
  :custom-face
  (line-number-mode ((t (:family unspecified))))
  (line-number-current-line ((t (:family unspecified))))
  :hook prog-mode)

(use-package display-fill-column-indicator
  :custom
  (display-fill-column-indicator-character ?\u2595) ;; RIGHT ONE EIGHT BLOCK
  (display-fill-column-indicator-column 80)
  :custom-face
  (fill-column-indicator
   ((t (:background unspecified :foreground "pink"))))
  :hook prog-mode)

(use-package eldoc
  :custom (eldoc-echo-area-use-multiline-p nil)
  :diminish eldoc-mode)

(use-package paren
  :custom (show-paren-mode nil)
  :hook (prog-mode . show-paren-local-mode))

(use-package simple
  :hook (text-mode . visual-line-mode))

(use-package tab-line
  :custom
  (global-tab-line-mode t)
  (tab-line-new-button-show nil))

(use-package files
  :custom
  (make-backup-files nil)
  (auto-save-default nil))

(use-package mwheel
  :custom
  (mouse-wheel-follow-mouse 't)
  (mouse-wheel-progressive-speed nil)
  (mouse-wheel-tilt-scroll t)
  (mouse-wheel-scroll-amount '(1 ((shift) . 1)))
  (mouse-wheel-scroll-amount-horizontal '(1 ((shift) . 1))))

(use-package pixel-scroll
  :custom (pixel-scroll-precision-mode t))

;; backup-directory-alist (let ((backup-directory (locate-user-emacs-file "backups/")))
;;                          (make-directory backup-directory t)
;;                          (list
;;                           (cons "." backup-directory)))
;; tls
;; gnutls-min-prime-bits 3072
;; network-security-level 'medium
