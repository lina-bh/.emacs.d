;; -*- lexical-binding: t; -*-
(eval-when-compile
  (require 'use-package))
(require 'bind-key)

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
  :ensure
  :demand t
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
  :ensure
  :custom (marginalia-mode t))

(use-package consult
  :ensure
  :custom
  (consult-async-split-style nil)
  (xref-show-xrefs-function #'consult-xref)
  :bind
  ("C-s" . consult-line)
  ("C-x b" . consult-buffer)
  ("C-x p b" . consult-project-buffer)
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
  :ensure
  :custom
  (corfu-quit-at-boundary nil)
  (global-corfu-mode t))

(use-package embark-consult :ensure :demand t)
(use-package embark
  :ensure
  :after embark-consult
  :init
  (unbind-key "C-h" 'help-map)
  :custom
  (embark-indicators '(embark-minimal-indicator
                       embark-highlight-indicator
                       embark-isearch-highlight-indicator))
  (prefix-help-command #'embark-prefix-help-command)
  :bind
  ("C-." . embark-act)
  ("M-." . embark-dwim)
  (:map minibuffer-local-map
        ("C-c" . embark-act)
        ("C-e" . embark-export)))
