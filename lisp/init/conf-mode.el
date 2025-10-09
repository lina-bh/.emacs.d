;; -*- lexical-binding: t; -*-
(use-package conf-mode
  :ensure nil
  :mode "\\.gitignore\\'"
  :mode ("\\.container\\'" . conf-desktop-mode)
  :mode ("\\.volume\\'" . conf-desktop-mode)
  :mode ("\\.network\\'" . conf-desktop-mode))
