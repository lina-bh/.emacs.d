;; -*- lexical-binding: t; -*-
(eval-when-compile
  (require 'use-package)
  (require 'bind-key))

(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file t)

(unless (boundp 'package-archives)
  (package-initialize))
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)

(use-package diminish
  :ensure
  :defer t)

(add-to-list 'load-path (locate-user-emacs-file "lisp/"))
(load-library "lina-core")
(load-library "lina-compl")
(load-library "lina-modes")
(load-library "lina-python")
(load-library "lina-check")
(load-library "lina-keys")
;; (load-library "lina-tex")
(load-library "lina-org")

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
  :defer t
  :custom (global-auto-revert-mode t)
  :diminish auto-revert-mode
  :bind
  ("C-c G" . #'magit)
  ("C-c g c" . #'magit-clone)
  ("C-c g d" . #'magit-diff-dwim))

(use-package vterm
  :ensure
  :defer t
  :custom ((vterm-always-compile-module t)
           (vterm-timer-delay 0.001)
	   (vterm-kill-buffer-on-exit nil)))

(use-package dired
  :commands dired-get-file-for-visit
  :custom ((delete-by-moving-to-trash t))
  :init
  (defun lina-dired-shellopen-at-point ()
    (interactive)
    (start-process "dired-open" nil "open" (dired-get-file-for-visit)))
  :hook (dired-mode . dired-hide-details-mode)
  :bind (:map dired-mode-map
              ("E" . #'lina-dired-shellopen-at-point)))
(use-package dired-single
  :ensure
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

(use-package auto-compile
  :ensure
  :defer t
  :hook (emacs-lisp-mode . turn-on-auto-compile-mode))

(use-package ess-site
  :defer t
  :ensure ess)

(pcase system-type
  ('windows-nt (load-library "lina-w32"))
  ('darwin (load-library "lina-macos")))
