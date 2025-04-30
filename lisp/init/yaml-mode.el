(use-package yaml-mode
  :ensure t
  :config
  (defun my-yaml-insert-item ()
    (interactive)
    (newline-and-indent)
    (yaml-electric-backspace 1)
    (insert "- ")
    (indent-for-tab-command))
  :hook (yaml-mode . puni-mode)
  :bind (:map yaml-mode-map
              ("M-RET" . my-yaml-insert-item)))
