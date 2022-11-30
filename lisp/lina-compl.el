;; -*- lexical-binding: t; -*-
(use-package vertico
  :ensure t
  :custom (vertico-count-format '("" . "%s/%s"))
  :init (vertico-mode t))

(use-package orderless
  :ensure t
  :custom (completion-styles '(orderless basic)))

(use-package consult
  :ensure t
  :custom (completion-in-region-function 'consult-completion-in-region))

(provide 'lina-compl)
