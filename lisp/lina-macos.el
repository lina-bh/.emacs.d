;; -*- lexical-binding: t; -*-
(eval-and-compile
  (require 'lina-package))

(use-package emacs
  :if (eq system-type 'darwin)
  :custom
  (dired-use-ls-dired nil)
  (mouse-wheel-flip-direction t)
  :custom-face
  (default ((((type ns)) (:family "Monaco" :height 115))))
  (fixed-pitch ((((type ns)) (:family "Monaco" :height 115))))
  (variable-pitch ((((type ns)) (:family "Lucida Grande" :height 140))))
  (fill-column-indicator ((((type ns)) (:family "Menlo" :foreground "pink"
                                                :background unspecified))))
  (tab-line ((((type ns)) (:family "Helvetica")))))

(use-package dash-at-point
  :straight t
  :bind (("C-c d" . dash-at-point)))
