;; -*- lexical-binding: t; -*-
(eval-when-compile
  (require 'use-package))

(package-initialize)
(setopt package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                           ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                           ("melpa" . "https://melpa.org/packages/")
                           ("gnu-devel" . "https://elpa.gnu.org/devel/"))
        package-priorities '(("gnu-devel" . -1))
	package-native-compile t)

(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file t)

(use-package auto-compile
  :ensure
  :demand t
  :custom (auto-compile-on-load-mode t)
  :hook (emacs-lisp-mode . turn-on-auto-compile-mode))

(use-package exec-path-from-shell
  :if (eq system-type 'darwin)
  :ensure
  :demand t
  :custom (exec-path-from-shell-arguments '("-l"))
  :config (exec-path-from-shell-initialize))

(use-package diminish
  :ensure
  :demand t)

(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))

(add-to-list 'load-path (locate-user-emacs-file "lisp/"))
(load "lina-core")
(load "lina-modes")
(load "lina-compl")
(load "lina-check")
(load "lina-org")
(load "lina-tex")

(defun delete-visited-file ()
  "Delete the file in the current buffer."
  (interactive)
  (let* ((buffer (current-buffer))
         (file-name (buffer-file-name buffer)))
    (if file-name
        (when (y-or-n-p (format "Delete %s?" file-name))
          (funcall-interactively #'delete-file file-name)
          (kill-buffer buffer))
      (message "Buffer not visiting any file"))))

(use-package magit
  :ensure
  :custom
  (magit-auto-revert-mode nil)
  (global-auto-revert-mode t)
  :diminish auto-revert-mode)

(use-package vterm
  :ensure
  :custom ((vterm-always-compile-module t)
           (vterm-timer-delay 0.001)
	   (vterm-kill-buffer-on-exit nil)))

(use-package dired
  :custom (delete-by-moving-to-trash t)
  :hook (dired-mode . dired-hide-details-mode))
(use-package dired-single
  :after (dired)
  :bind (:map dired-mode-map
	      ([remap dired-find-file]
	       .
	       dired-single-buffer)
	      ([remap dired-mouse-find-file-other-window]
	       .
	       dired-single-buffer-mouse)
	      ([remap dired-up-directory]
	       .
	       dired-single-up-directory)))

(use-package ess-site
  :ensure ess)

(pcase system-type
  ;; ('windows-nt (load "lina-w32"))
  ('darwin (load "lina-macos")))
