(use-package envrc
  :ensure t
  :custom
  (envrc-global-mode t)
  (envrc-debug t)
  (envrc-none-lighter nil)
  (envrc-show-summary-in-minibuffer nil)
  :init
  (defalias 'direnv-reload #'envrc-reload)
  (defalias 'direnv-allow #'envrc-allow))
