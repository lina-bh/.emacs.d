;; -*- lexical-binding: t; -*-
(eval-and-compile
  (require 'lina-package))

(use-package savehist
  :custom (savehist-mode t))
(use-package recentf
  :custom
  (recentf-mode t)
  (recentf-max-menu-items 80))
(use-package bookmark
  :custom
  (bookmark-save-flag 1)
  (bookmark-fringe-mark nil))

(use-package orderless
  :straight t
  :custom (completion-styles '(orderless basic)))

(use-package icomplete
  :init
  (defun lina-icomplete-minibuf-hook ()
    (setq-local truncate-lines t
                completion-auto-help nil))
  :custom
  (icomplete-matches-format "")
  (icomplete-show-matches-on-no-input t)
  (fido-mode t)
  (fido-vertical-mode t)
  :hook (icomplete-minibuffer-setup . lina-icomplete-minibuf-hook))

(use-package marginalia
  :straight t
  :custom (marginalia-mode t))

(use-package consult
  :straight t
  :custom
  (consult-async-split-style nil)
  (xref-show-xrefs-function #'consult-xref)
  :bind
  ("C-s" . consult-line)
  ("C-x b" . consult-buffer)
  ("C-x r b" . consult-bookmark)
  ("C-x p f" . consult-find)
  ("C-x p g" . consult-ripgrep))
(use-package consult-org
  :after org consult
  :bind (:map org-mode-map
              (("C-c C-j" . consult-org-heading))))
(use-package consult-info
  :after info consult
  :bind ("C-h i" . consult-info))
(use-package consult-imenu
  :after imenu consult
  :bind ("M-g i" . consult-imenu))

(use-package corfu
  :straight t
  :custom
  (corfu-quit-at-boundary nil)
  (global-corfu-mode t))

(use-package embark-consult
  :straight t)
(use-package embark
  :straight t
  :after embark-consult
  :custom
  (embark-indicators '(embark-minimal-indicator
                       embark-highlight-indicator
                       embark-isearch-highlight-indicator))
  (prefix-help-command #'embark-prefix-help-command)
  :config
  (unbind-key "C-h" 'help-map)
  :bind
  ("C-." . embark-act)
  ("M-." . embark-dwim)
  (:map minibuffer-local-map
        ("C-c" . embark-act)
        ("C-e" . embark-export)))

