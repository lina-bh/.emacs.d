;; -*- lexical-binding: t; -*-
(eval-when-compile
  (require 'use-package))
(require 'bind-key)

(use-package emacs
  :if (eq system-type 'darwin)
  :custom
  (dired-use-ls-dired nil)
  (mouse-wheel-flip-direction t)
  :config
  (setq source-directory (expand-file-name "./emacs/" (getenv "HOME")))
  :custom-face
  (default ((((type ns)) (:family "Monaco" :height 115))))
  (fixed-pitch ((((type ns)) (:family "Monaco" :height 115))))
  (variable-pitch ((((type ns)) (:family "Lucida Grande" :height 140))))
  (fill-column-indicator ((((type ns)) (:family "Menlo" :foreground "pink"
                                                :background unspecified))))
  (tab-line ((((type ns)) (:family "Helvetica")))))

(use-package dired
  :if (eq system-type 'darwin)
  :custom
  (dired-guess-shell-alist-user
   '(("\\.pdf\\'" "open -a Skim.app")
     ("" "open"))))

(use-package exec-path-from-shell
  :if (eq system-type 'darwin)
  ;; :ensure
  ;; demand t
  :config (setq exec-path-from-shell-arguments '("-l"))
  ;; :config (exec-path-from-shell-initialize)
  )

(use-package dash-at-point
  :if (eq system-type 'darwin)
  :ensure
  :bind ("C-c d" . dash-at-point))
