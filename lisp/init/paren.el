(use-package paren
  :ensure nil
  :custom
  (show-paren-mode nil)
  :hook ((prog-mode conf-mode yaml-mode) . show-paren-local-mode))
