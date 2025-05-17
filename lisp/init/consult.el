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
  (defun consult-grep- ()
    (interactive)
    (call-interactively
     (or
      (and (executable-find "rg" 'remote)
           #'consult-ripgrep)
      #'consult-grep)))
  (defun consult-find- ()
    (interactive)
    (call-interactively
     (or
      (and (or (executable-find "fd" 'remote)
               (executable-find "fdfind" 'remote))
           #'consult-fd)
      #'consult-find)))
  :bind
  ("M-s" . consult-line)
  ("M-g" . consult-imenu)
  ("M-y" . consult-yank-pop)
  ("C-x g" . consult-grep-)
  ("C-x f" . consult-find-)
  ("C-x b" . consult-buffer)
  ("C-h i" . consult-info))
