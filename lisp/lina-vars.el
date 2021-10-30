;; -*- lexical-binding: t; -*-
(setq-default
 mouse-wheel-scroll-amount '(1 ((shift) . 1))
 mouse-wheel-progressive-speed nil
 mouse-wheel-follow-mouse 't
 scroll-step 1
 scroll-preserve-screen-position t
 create-lockfiles nil
 make-backup-files nil
 inhibit-splash-screen t
 vc-follow-symlinks t
 frame-title-format "%b"
 w32-enable-synthesized-fonts t
 load-prefer-newer t
 gc-cons-threshold 1600000
 display-fill-column-indicator-column 80
 display-fill-column-indicator-character ?\s
 initial-frame-alist '((width . 90))
 default-frame-alist initial-frame-alist
 gnutls-min-prime-bits 3072
 network-security-level 'high
 ring-bell-function 'ignore
 native-comp-async-report-warnings-errors nil
 )
(defalias 'yes-or-no-p 'y-or-n-p)

(provide 'lina-vars)
