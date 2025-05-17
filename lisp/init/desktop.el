(use-package desktop
  :ensure nil
  :custom
  (desktop-save-mode nil)
  (desktop-save t)
  (desktop-dir-name user-emacs-directory)
  (desktop-load-locked-desktop 'check-pid))
