;; -*- lexical-binding: t; -*-
;; (remove-hook 'find-file-hook 'vc-find-file-hook)
(setq load-prefer-newer t
      use-package-enable-imenu-support t
      package-enable-at-startup nil)
;; vc-handled-backends nil

(modify-all-frames-parameters
 `((tty-color-mode . -1)
   (height . 57)
   (width . 99)
   (tool-bar-lines . 0)
   (menu-bar-lines . ,(if (and window-system
                               (eq system-type 'darwin))
                          1 0))))

(setq straight-host-usernames '((github . "lina-bh")))
(defvar bootstrap-version 7)
(load (locate-user-emacs-file "straight/repos/straight.el/bootstrap.el") nil
      'nomessage)

(straight-use-package '(auto-compile))
(require 'auto-compile)
(auto-compile-on-load-mode)

(straight-use-package '(benchmark-init))
(unless noninteractive
  (require 'benchmark-init-autoloads)
  (benchmark-init/activate)
  (add-hook 'emacs-startup-hook #'benchmark-init/deactivate))
