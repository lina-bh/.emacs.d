(use-package consult
  :ensure t
  :custom
  (consult-async-split-style nil)
  (consult-find-args "find .")
  (consult-line-start-from-top t)
  (consult-preview-allowed-hooks '())
  (consult-preview-max-count 0)
  (completion-in-region-function #'consult-completion-in-region)
  (xref-show-definitions-function #'consult-xref)
  (xref-show-xrefs-function #'consult-xref)
  :config
  (define-advice consult-grep
      (:around (consult-grep &rest args) ripgrep)
    (apply (or (and (executable-find "rg" 'remote) 'consult-ripgrep)
               consult-grep)
           args))
  (define-advice consult-find
      (:around (consult-find &rest args) fd)
    (apply (or (and (executable-find "fd" 'remote) 'consult-fd)
               consult-find)
           args))
  :bind
  ("C-s" . consult-line)
  ("M-g" . consult-imenu)
  ("M-y" . consult-yank-pop)
  ("C-x p g" . consult-grep)
  ("C-x p f" . consult-find)
  ("C-x b" . consult-buffer)
  ("C-b" . consult-buffer)
  ("M-f" . consult-flymake)
  ("C-h i" . consult-info))
