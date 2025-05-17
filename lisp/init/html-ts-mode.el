;; -*- lexical-binding: t; -*-
(use-package html-ts-mode
  :ensure nil
  :config
  (defun lina-html-ts-hook ()
    (display-line-numbers-mode))
  :hook (html-ts-mode . lina-html-ts-hook)
  :bind (:map html-mode-map
              ("C-c C-c" . recompile))
  :mode "\\.html\\'")
