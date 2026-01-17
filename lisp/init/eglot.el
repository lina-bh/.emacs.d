(use-package eglot
  :ensure t
  :custom
  (eglot-ignored-server-capabilities '(:inlayHintProvider))
  (eglot-report-progress nil)
  :config
  (setq-default eglot-server-programs
                (list
                 (cons 'nix-mode (eglot-alternatives
                                  '("nil" "rnix-lsp" "nixd")))
                 '(dockerfile-ts-mode "docker-langserver" "--stdio")
                 '(go-ts-mode "gopls")
                 (list 'rust-mode "rust-analyzer")))
  (defun my-eglot-hook ()
    (setq-local eldoc-echo-area-use-multiline-p (eglot-managed-p))
    (when (memq major-mode '(dockerfile-ts-mode))
      (add-hook 'before-save-hook #'eglot-format-buffer nil 'local)))
  :hook (eglot-managed-mode . my-eglot-hook)
  :bind (:map eglot-mode-map
              ("C-l" . eglot-format)
              ("<f2>" . eglot-rename)))
