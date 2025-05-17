;; -*- lexical-binding: t; -*-
(use-package vterm
  :ensure t
  :commands project-vterm
  :custom
  (vterm-shell "fish")
  (vterm-kill-buffer-on-exit t)
  (vterm-timer-delay 0.01)
  :config
  (defun project-vterm (&optional prefix)
    (interactive "P")
    (let ((default-directory (project-root (project-current t)))
          (vterm-buffer-name (project-prefixed-buffer-name "vterm")))
      (vterm prefix)))
  :bind
  ("C-x p t" . project-vterm)
  (:map vterm-mode-map
        ("M-w" . nil)
        ("C-y" . nil)
        ("M-." . nil)
        ("M-:" . nil)
        ("C-u" . vterm--self-insert)
        ("C-c C-c" . vterm--self-insert)
        ("C-t" . vterm-copy-mode))
  (:map vterm-copy-mode-map
        ("q" . vterm-copy-mode-done)))
