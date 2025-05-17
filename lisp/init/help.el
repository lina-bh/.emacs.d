;; -*- lexical-binding: t; -*-
(use-package help
  :ensure nil
  :custom
  (help-window-select t)
  :bind
  (("C-h ," . customize-variable)
   ("C-h c" . describe-char)
   ("C-h g" . customize-group)
   ("C-h s" . info-lookup-symbol)
   ("C-h C-h" . nil)))
