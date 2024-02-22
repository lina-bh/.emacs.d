;; -*- lexical-binding: t; -*-
(eval-when-compile
  (require 'profiler))

;; (unless noninteractive
;;   (setq profiler-sampling-interval 500000)
;;   (profiler-start 'cpu))

;; (remove-hook 'find-file-hook 'vc-find-file-hook)
(setq load-prefer-newer t
      gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 1
      use-package-enable-imenu-support t)
;; vc-handled-backends nil
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold (* 8 (expt 1024 2))
                  gc-cons-percentage 0.1)))

(add-to-list 'load-path (locate-user-emacs-file "site-lisp/auto-compile"))
(when (require 'auto-compile nil t)
  (auto-compile-on-load-mode t))

(push '(tool-bar-lines . 0) default-frame-alist)

;; (when (profiler-running-p)
;;   (profiler-stop)
;;   (setq profiler-sampling-interval 1000000))
