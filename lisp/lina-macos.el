;; -*- lexical-binding: t; -*-
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
  :ensure
  :bind (("C-c d" . dash-at-point)))
