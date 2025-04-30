(use-package markdown-mode
  :ensure t
  :custom
  (markdown-command "pandoc -t html5")
  :bind (:map markdown-mode-map
              ("C-c C-c" . recompile)))
