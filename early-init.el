;; -*- lexical-binding: t; no-byte-compile: t; -*-
(defconst lina-gc-bytes (* 64 1024 1024))
(setq load-prefer-newer t
      gc-cons-threshold most-positive-fixnum
      vc-handled-backends nil
      use-package-enable-imenu-support t)

;; https://github.com/doomemacs/doomemacs/blob/master/lisp/doom.el#L361
(put 'file-name-handler-alist 'lina-saved file-name-handler-alist)
(setq file-name-handler-alist (list (rassq 'jka-compr-handler
                                           file-name-handler-alist)))

;; https://github.com/doomemacs/doomemacs/blob/master/lisp/doom.el#L405
;; https://github.com/doomemacs/doomemacs/blob/master/lisp/doom.el#L492
;; (advice-add #'tool-bar-setup :override #'ignore)
;; (advice-add #'display-startup-screen :override #'ignore)
(advice-add #'display-startup-echo-area-message :override #'ignore)
;; (defun lina-tool-bar-mode-before ()
;;   (advice-remove #'tool-bar-setup #'ignore)
;;   (tool-bar-setup))
;; (add-hook 'tool-bar-mode-hook #'lina-tool-bar-mode-before)

;; https://github.com/doomemacs/doomemacs/blob/master/lisp/doom.el#L466
;; (put 'mode-line-format 'lina-saved mode-line-format)
;; (setq mode-line-format nil)

(define-advice startup--load-user-init-file (:after (&rest _))
  (setq gc-cons-threshold lina-gc-bytes
        mode-line-format (get 'mode-line-format 'lina-saved)
        file-name-handler-alist (get 'file-name-handler-alist 'lina-saved)))

(tool-bar-mode -1)
(menu-bar-mode -1)
(defun my-set-frame-params ()
  ;; (tty-color-mode . -1)
  (modify-all-frames-parameters
   `(
     ;; (height . 60)
     ;; (width . 180)
     (tool-bar-lines . 0)
     (menu-bar-lines . 0)
     ,@(if (and window-system
                (eq system-type 'darwin))
           '((ns-appearance . light)))))
  (set-scroll-bar-mode 'right))
(my-set-frame-params)
(add-hook 'after-init-hook #'my-set-frame-params)
