;; -*- lexical-binding: t; no-byte-compile: t; -*-
(defconst lina-gc-bytes (* 64 1024 1024))
(setq load-prefer-newer t
      gc-cons-threshold most-positive-fixnum
      vc-handled-backends '(Git)
      use-package-enable-imenu-support t
      force-load-messages t)
(when emacs-repository-version
  debug-on-error t)
;; https://github.com/doomemacs/doomemacs/blob/master/lisp/doom.el#L361
(put 'file-name-handler-alist 'lina-saved file-name-handler-alist)
(setq file-name-handler-alist `(,(rassq 'jka-compr-handler
                                        file-name-handler-alist)))
;; https://github.com/doomemacs/doomemacs/blob/master/lisp/doom.el#L405
;; https://github.com/doomemacs/doomemacs/blob/master/lisp/doom.el#L492
(advice-add #'tool-bar-setup :override #'ignore)
(advice-add #'display-startup-screen :override #'ignore)
(advice-add #'display-startup-echo-area-message :override #'ignore)
(defun lina-tool-bar-mode-before ()
  (advice-remove #'tool-bar-setup #'ignore)
  (tool-bar-setup))

;; https://github.com/doomemacs/doomemacs/blob/master/lisp/doom.el#L466
(put 'mode-line-format 'lina-saved mode-line-format)
(setq mode-line-format nil
      ;; inhibit-message t
      )

(defun lina-after-init (&rest _)
  (setq gc-cons-threshold lina-gc-bytes
        inhibit-message nil
        mode-line-format (get 'mode-line-format 'lina-saved)
        file-name-handler-alist (get 'file-name-handler-alist 'lina-saved))
  (add-hook 'tool-bar-mode-hook #'lina-tool-bar-mode-before))
(advice-add #'startup--load-user-init-file :after #'lina-after-init)

(modify-all-frames-parameters
 `((tty-color-mode . -1)
   (height . 48)
   (width . 99)
   (tool-bar-lines . 0)
   (menu-bar-lines . ,(if (and window-system
                               (eq system-type 'darwin))
                          1 0))))
(setq tool-bar-mode nil)
