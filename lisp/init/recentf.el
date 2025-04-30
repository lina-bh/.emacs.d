(use-package recentf
  :ensure nil
  :custom
  (recentf-mode t)
  (recentf-max-menu-items most-positive-fixnum)
  (recentf-max-saved-items 80)
  :bind
  ("C-x C-r" . recentf-open)
  ("C-r" . recentf-open))
