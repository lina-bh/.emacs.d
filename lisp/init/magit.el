;; -*- lexical-binding: t; -*-
(use-package transient
  :ensure t
  :custom (transient-display-buffer-action
           '(display-buffer-at-bottom
             (dedicated . t)
             (inhibit-same-window . t))))

(use-package magit
  :ensure t
  :custom
  (magit-display-buffer-function #'display-buffer)
  (magit-commit-show-diff nil)
  :init
  (magit-auto-revert-mode t)
  :bind
  ("C-x g" . magit))
