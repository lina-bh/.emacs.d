(use-package display-fill-column-indicator
  :ensure nil
  :custom
  (display-fill-column-indicator-character ?\u2595) ;; RIGHT ONE EIGHT BLOCK
  :hook (prog-mode . display-fill-column-indicator-mode))
