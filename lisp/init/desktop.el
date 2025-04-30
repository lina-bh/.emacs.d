(use-package desktop
  :ensure nil
  :custom
  (desktop-save-mode t)
  (desktop-save t)
  (desktop-dir-name user-emacs-directory)
  (desktop-load-locked-desktop 'check-pid))
