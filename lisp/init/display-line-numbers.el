(use-package display-line-numbers
  :ensure nil
  :custom
  (display-line-numbers-grow-only t)
  (display-line-numbers-width-start 1000)
  :hook ((prog-mode conf-mode yaml-mode) . display-line-numbers-mode))
