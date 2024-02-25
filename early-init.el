;; -*- lexical-binding: t; -*-
;; (remove-hook 'find-file-hook 'vc-find-file-hook)
(setq load-prefer-newer t
      use-package-enable-imenu-support t)
;; vc-handled-backends nil

(add-to-list 'load-path (locate-user-emacs-file "site-lisp/auto-compile"))
(when (require 'auto-compile nil t)
  (auto-compile-on-load-mode t))

(modify-all-frames-parameters
 `((tty-color-mode . -1)
   (height . 57)
   (width . 99)
   (tool-bar-lines . 0)
   (menu-bar-lines . ,(if (and window-system
                               (eq system-type 'darwin))
                          1 0))))
