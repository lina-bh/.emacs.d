;; -*- lexical-binding: t; -*-
(use-package savehist
  :custom (savehist-mode t))
(use-package saveplace
  :custom (save-place-mode t))
(use-package bookmark
  :custom
  (bookmark-save-flag 1)
  (bookmark-fringe-mark nil))
(use-package recentf
  :custom
  (recentf-max-menu-items most-positive-fixnum)
  (recentf-max-saved-items 80)
  (recentf-mode t)
  :bind ("C-x C-r" . #'recentf-open))
(use-package xref
  :custom
  (xref-search-program 'ripgrep)
  (xref-prompt-for-identifier nil)
  :bind ("M-/" . xref-find-definitions))

(use-package orderless
  :demand t
  :custom (orderless-component-separator " +\\|[-/]")
  :config (setq-default completion-styles '(orderless basic)))

(use-package vertico
  :custom
  (vertico-mode t)
  (vertico-count-format nil)
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)))

(use-package marginalia
  :custom (marginalia-mode t))

(use-package consult
  :functions consult-completion-in-region
  :custom
  (consult-async-split-style nil)
  (consult-find-args "find .")
  (consult-line-start-from-top t)
  (consult-preview-allowed-hooks '())
  (completion-in-region-function #'consult-completion-in-region)
  (xref-show-definitions-function #'consult-xref)
  (xref-show-xrefs-function #'consult-xref)
  :bind
  ("M-g" . consult-imenu)
  ("C-x p f" . consult-find)
  ("C-x p g" . consult-ripgrep)
  ("C-h i" . consult-info))

(use-package helpful
  :custom (helpful-max-buffers nil)
  :hook (helpful-mode . show-paren-local-mode)
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-command] . helpful-command)
  (:map helpful-mode-map
        ("l" . quit-window)))

(use-package embark
  :demand t
  :init
  :custom
  (embark-indicators '(embark-minimal-indicator
                       embark-highlight-indicator
                       embark-isearch-highlight-indicator))
  (embark-cycle-key "<tab>")
  (prefix-help-command #'embark-prefix-help-command)
  :config
  (unbind-key "C-h" 'help-map)
  (delete 'embark-target-flymake-at-point embark-target-finders)
  (push '(identifier . xref-find-definitions) embark-default-action-overrides)
  (push '(eglot-code-actions embark--ignore-target)
        embark-target-injection-hooks)
  :bind
  ("C-." . embark-act)
  ("M-." . embark-dwim)
  ("C-h b" . embark-bindings)
  (:map embark-identifier-map
        ("2" . eglot-rename)
        ("l" . eglot-code-actions))
  (:map embark-symbol-map
        ("h" . helpful-symbol)))

(use-package corfu
  :custom
  (global-corfu-mode t)
  (corfu-quit-no-match nil))

(use-package cape
  :config
  (defun my-elisp-cape-hook ()
    (add-hook 'completion-at-point-functions #'cape-elisp-symbol -1 t))
  :hook ((emacs-lisp-mode inferior-emacs-lisp-mode) . my-elisp-cape-hook)
  :bind ("C-x a" . cape-abbrev))

(use-package icomplete
  :disabled t
  :init
  (defun my-icomplete-hook ()
    (setq-local truncate-lines t
                completion-auto-help nil
                completion-styles (default-value 'completion-styles)))
  (fido-vertical-mode t)
  (icomplete-matches-format "")
  (icomplete-show-matches-on-no-input t)
  (icomplete-compute-delay 0)
  :hook (icomplete-minibuffer-setup . my-icomplete-hook)
  :bind (:map icomplete-fido-mode-map
              ("TAB" . icomplete-fido-ret)
              ("<tab>" . icomplete-fido-ret)))

(use-package dumb-jump
  :config
  (defun my-dumb-jump-c-mode-hook ()
    (add-hook 'xref-backend-functions #'dumb-jump-xref-activate 0 t))
  :custom
  (dumb-jump-prefer-searcher 'rg)
  :hook (c-mode . my-dumb-jump-c-mode-hook))
