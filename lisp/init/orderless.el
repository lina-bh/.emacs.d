(use-package orderless
  :ensure t
  :demand t
  :custom (orderless-component-separator " +\\|[-/]")
  :config (setopt completion-styles '(orderless basic)))
