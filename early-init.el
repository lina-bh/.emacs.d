;; -*- lexical-binding: t; -*-
(defvar use-package-enable-imenu-support t)
(setq load-prefer-newer t
      package-enable-at-startup nil)

(modify-all-frames-parameters
 `((tty-color-mode . -1)
   (height . 57)
   (width . 99)
   (tool-bar-lines . 0)
   (menu-bar-lines . ,(if (and window-system
                               (eq system-type 'darwin))
                          1 0))))
