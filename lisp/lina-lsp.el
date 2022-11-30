;; -*- lexical-binding: t; -*-
(use-package eglot
  :ensure t
  :bind (:map eglot-mode-map
	      ("C-c 2" . #'eglot-rename))
  ;;:custom
  ;; (eglot-stay-out-of '(eldoc))
  :init
  (add-hook 'eglot-managed-mode-hook
	    (defun lina/eglot-managed-mode-hook ()
	      (put 'eglot-note 'flymake-overlay-control nil)
	      (put 'eglot-warning 'flymake-overlay-control nil)
	      (put 'eglot-error 'flymake-overlay-control nil))))

(provide 'lina-lsp)
