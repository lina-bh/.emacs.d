(use-package help
  :ensure nil
  :bind
  (("C-h ," . customize-variable)
   ("C-h c" . describe-char)
   ("C-h g" . customize-group)
   ("C-h s" . info-lookup-symbol)
   ("C-h C-f" . describe-face)
   ("C-h M" . describe-keymap)
   ("C-h C-h" . nil)))
