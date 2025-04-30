(use-package corfu
  :ensure t
  :custom
  (corfu-quit-no-match nil)
  (global-corfu-mode t)
  (global-corfu-modes '((not inferior-python-mode) t)))
