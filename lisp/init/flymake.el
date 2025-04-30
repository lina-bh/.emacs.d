(use-package flymake
  :ensure nil
  :hook
  (flymake-diagnostics-buffer-mode . visual-line-mode)
  ((sh-base-mode python-mode) . flymake-mode)
  :bind (:map project-prefix-map
              ("q" . flymake-show-project-diagnostics)))
