(use-package icomplete
  :ensure nil
  :custom
  (fido-vertical-mode t)
  (icomplete-matches-format "")
  (icomplete-show-matches-on-no-input t)
  (icomplete-compute-delay 0)
  :config
  (setq-mode-local minibuffer-mode
                   completion-auto-help nil)
  (defun my-icomplete-hook ()
    (setq-local truncate-lines t
                completion-styles (default-value 'completion-styles)))
  :hook (icomplete-minibuffer-setup . my-icomplete-hook)
  :bind
  (:map icomplete-minibuffer-map
        ("C-." . nil))
  (:map icomplete-fido-mode-map
        ("TAB" . icomplete-fido-ret)
        ("<tab>" . icomplete-fido-ret)))
