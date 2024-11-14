;; -*- lexical-binding: t; no-byte-compile: t; -*-
(setq load-prefer-newer t
      gc-cons-threshold most-positive-fixnum
      vc-handled-backends nil
      use-package-enable-imenu-support t)

(tool-bar-mode -1)
(menu-bar-mode -1)
(set-scroll-bar-mode 'right)
(defun my-set-frame-params ()
  (modify-all-frames-parameters
   '((height . 42)
     (width . 90))))
(my-set-frame-params)
(add-hook 'after-init-hook #'my-set-frame-params)
