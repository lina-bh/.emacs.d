;; -*- lexical-binding: t; no-byte-compile: t; -*-
(defconst lina-gc-bytes (* 64 1024 1024))
(setq load-prefer-newer t
      gc-cons-threshold most-positive-fixnum
      vc-handled-backends nil
      use-package-enable-imenu-support t)

(tool-bar-mode -1)
(menu-bar-mode -1)
(defun my-set-frame-params ()
  (modify-all-frames-parameters
   `((width . 85)))
  (set-scroll-bar-mode 'right))
(my-set-frame-params)
(add-hook 'after-init-hook #'my-set-frame-params)
