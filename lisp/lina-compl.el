;; -*- lexical-binding: t; -*-
(eval-when-compile
  (require 'use-package)
  (require 'bind-key))

(use-package savehist
  :custom (savehist-mode t))
(use-package recentf
  :custom
  (recentf-mode t)
  (recentf-max-menu-items 40)
  :bind ("C-x C-r" . #'recentf-open))
(use-package ffap
  :bind (("C-x C-d" . #'dired-at-point)
         ("C-x C-f" . #'find-file-at-point)))

(use-package orderless
  :straight t
  :custom (completion-styles '(orderless basic)))
(use-package marginalia
  :straight t
  :custom (marginalia-mode t))
(use-package vertico
  :straight t
  :custom
  (vertico-count-format '("" . "%s/%s"))
  (vertico-mode t)
  :bind
  (:map vertico-map
        ("DEL" . #'vertico-directory-delete-char)))

(use-package consult
  :straight t
  :custom
  (consult-async-split-style nil)
  :bind
  ("C-x p f" . #'consult-find)
  ("C-x p g" . #'consult-ripgrep)
  ("M-g i" . #'consult-imenu))
(use-package consult-org
  :after org consult
  :bind (:map org-mode-map
              (("C-c C-j" . #'consult-org-heading))))
(use-package consult-xref
  :after xref consult
  :custom
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref))
(use-package consult-info
  :after info consult
  :bind ("C-h i" . #'consult-info))
(use-package corfu
  :straight t
  :custom (global-corfu-mode t))

(use-package embark
  :straight t
  :bind
  (:map minibuffer-local-map
        ("C-e" . #'embark-export)
        ("C-s" . #'embark-collect)))
(use-package embark-consult
  :straight t
  :after embark)
