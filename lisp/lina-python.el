;; -*- lexical-binding: t; -*-
(use-package python
  :init
  (defun lina-python-mode-hook ()
    (setq-local tab-always-indent t))
  :custom
  (python-indent-guess-indent-offset-verbose nil)
  (python-shell-dedicated 'project)
  :hook (python-mode . lina-python-mode-hook))

(use-package ruff-format
  :bind (:map python-mode-map
              ("C-c f" . ruff-format-buffer)))

;; (use-package pyvenv
;;   :disabled t
;;   :defines pyvenv-virtual-env
;;   :commands pyvenv-mode pyvenv-tracking-mode
;;   :init
;;   (defun lina-pyvenv-activate-hook ()
;;     (setq-local python-shell-interpreter
;;                 (expand-file-name "bin/python3" pyvenv-virtual-env)))
;;   (pyvenv-mode)
;;   (pyvenv-tracking-mode))
