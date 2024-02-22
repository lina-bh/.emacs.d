;; -*- lexical-binding: t; -*-
(eval-when-compile
  (require 'use-package)
  (require 'bind-key))

(use-package savehist
  :custom (savehist-mode t))
(use-package orderless
  :ensure
  :custom (completion-styles '(orderless basic)))
(use-package marginalia
  :ensure
  :custom (marginalia-mode t))
(use-package vertico
  :ensure
  :custom
  (vertico-count-format '("" . "%s/%s"))
  (vertico-mode t))
(use-package consult
  :ensure
  ;; :custom (consult-preview-excluded-files '("\\`/[^/|:]+:" "\\.org\\'"))
  :bind (("C-x b" . #'consult-buffer)
         ("C-x p f" . #'consult-find)
         ("C-x p g" . #'consult-ripgrep)
         ("M-g i" . #'consult-imenu)))
(use-package consult-org
  :after org consult
  :bind (:map org-mode-map
              (("C-c C-j" . #'consult-org-heading))))
(use-package consult-xref
  :after xref consult
  :custom (xref-show-xrefs-function #'consult-xref))
(use-package consult-info
  :after info consult
  :bind (("C-h g" . #'consult-info)))
(use-package corfu
  :ensure
  :custom (global-corfu-mode t))
