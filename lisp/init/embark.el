(use-package embark-consult :ensure t)

(use-package embark
  :ensure t
  :custom
  (embark-indicators '(embark-minimal-indicator
                       embark-highlight-indicator
                       embark-isearch-highlight-indicator))
  (embark-cycle-key "<tab>")
  (prefix-help-command #'embark-prefix-help-command)
  :config
  (delete 'embark-target-flymake-at-point embark-target-finders)
  (push '(identifier . xref-find-definitions) embark-default-action-overrides)
  (push '(eglot-code-actions embark--ignore-target)
        embark-target-injection-hooks)
  :bind
  ("C-." . embark-act)
  ("M-." . embark-dwim)
  ("C-h b" . embark-bindings)
  (:map embark-identifier-map
        ("2" . eglot-rename)
        ("1" . eglot-code-actions))
  (:map minibuffer-mode-map
        ("C-S-x" . embark-export)))
