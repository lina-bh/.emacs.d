;; -*- lexical-binding: t; -*-
(eval-when-compile
  (require 'use-package))
(setq use-package-always-defer t)

(setq inhibit-message-regexps '("^Loading "))
(push #'inhibit-message set-message-functions)

(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file t t t t)
(load (locate-user-emacs-file "path.el") t nil t t)

(use-package package
  :custom
  (package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                      ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                      ("melpa" . "https://melpa.org/packages/")
                      ("gnu-devel" . "https://elpa.gnu.org/devel/")))
  (package-priorities '(("gnu-devel" . -1))))
(use-package bind-key)

;; (use-package benchmark-init
;;   :disabled t
;;   :init (benchmark-init/activate)
;;   :hook (emacs-startup . benchmark-init/deactivate))

(use-package diminish
  :ensure)

(when (and (< emacs-major-version 30)
           (not (package-installed-p 'vc-use-package)))
  (package-vc-install "https://github.com/slotThe/vc-use-package"))

(add-to-list 'load-path (locate-user-emacs-file "lisp/"))

(load "lina-core")
(load "lina-modes")
(load "lina-compl")
(load "lina-check")
(load "lina-tools")
(load "lina-org")
(load "lina-tex")
(load "lina-poly")
(pcase system-type
  ('darwin (load "lina-macos")))

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

;; (use-package auto-compile
;;   :disabled t
;;   :ensure
;;   :demand t
;;   :custom (auto-compile-on-load-mode t)
;;   ;; :hook (emacs-lisp-mode . turn-on-auto-compile-mode)
;;   )

;; (use-package undo-tree
;;   :disabled t
;;   :ensure
;;   :diminish undo-tree-mode
;;   :custom
;;   (undo-tree-auto-save-history nil)
;;   (global-undo-tree-mode t))
