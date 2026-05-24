;; -*- lexical-binding: t; -*-
(autoload 'setq-mode-local "mode-local")

;;; initialisation & general behaviour
(setq-default custom-file (locate-user-emacs-file "custom.el")
              use-package-always-defer t
              use-package-enable-imenu-support t
              package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                                 ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                                 ("melpa" . "https://melpa.org/packages/")
                                 ("melpa-stable" . "https://stable.melpa.org/packages/"))
              package-archive-priorities '(("gnu" . 2)
                                           ("nongnu" . 1)
                                           ("melpa" . -1))
              package-install-upgrade-built-in t)
(load custom-file t)

(use-package emacs
  :ensure nil
  :preface
  (defconst linux-font '(:family "Iosevka Nerd Font" :height 105))
  :custom
  (auth-sources '("~/.authinfo"))
  (auto-save-default nil)
  (create-lockfiles nil)
  (delete-selection-mode t)
  (eldoc-minor-mode-string nil)
  (enable-recursive-minibuffers t)
  (garbage-collection-messages t)
  (find-function-mode t)
  (indent-tabs-mode nil)
  (inhibit-startup-screen t)
  (initial-scratch-message nil)
  (initial-major-mode 'fundamental-mode)
  (make-backup-files nil)
  (menu-bar-mode nil)
  (mouse-autoselect-window t)
  (repeat-mode t)
  (require-final-newline t)
  (savehist-mode t)
  (save-place-mode t)
  (save-interprogram-paste-before-kill t)
  (tool-bar-mode nil)
  (use-short-answers t)
  (view-read-only t)
  (warning-minimum-level :emergency)
  (xterm-mouse-mode t)
  :custom-face
  (default ((((type x pgtk)) ,linux-font)))
  (fixed-pitch ((((type x pgtk)) ,linux-font)))
  (fixed-pitch-serif ((t :inherit (fixed-pitch))))
  :hook
  (after-init . (lambda ()
                  (garbage-collect)
                  (setq gc-cons-threshold (* (expt 1024 2) 16))))
  (prog-mode . delete-trailing-whitespace-mode)
  :bind
  ("M-u" . ignore)
  ("C-z" . undo)
  ("M-," . pop-to-mark-command)
  ("C-," . pop-global-mark)
  ("M-;" . comment-line)
  ("M-<left>" . backward-sexp)
  ("M-<right>" . forward-sexp)
  ("M-<up>" . backward-up-list)
  ("M-<down>" . down-list)
  (:map ctl-x-map
        ("x" . revert-buffer-quick)))

;;; look & feel

(use-package window
  :ensure nil
  :custom
  (display-buffer-base-action '(nil))
  (display-buffer-alist
   `(((derived-mode . magit-revision-mode)
      display-buffer-reuse-mode-window
      (mode . magit-log-mode))
     ((or
       (category . comint)
       (category . warning)
       (derived-mode . flymake-diagnostics-buffer-mode)
       (derived-mode . eshell-mode)
       (derived-mode . help-mode)
       (derived-mode . term-mode)
       (derived-mode . inferior-emacs-lisp-mode)
       ,(rx bos "*" (or "trace-output")))
      display-buffer-in-side-window
      (window-height . 18)
      (slot . 0))
     ("*Completions*"
      (display-buffer-reuse-window
       display-buffer-at-bottom))
     ((or
       ,(rx bos "*Customize"))
      (display-buffer-reuse-mode-window))
     (,(rx bos "*Pp")
      (display-buffer-below-selected))))
  (switch-to-buffer-in-dedicated-window 'pop)
  (switch-to-buffer-obey-display-actions t)
  (split-height-threshold nil)
  (split-window-preferred-direction 'horizontal))

(use-package tooltip
  :ensure nil
  :custom
  (tooltip-delay 0.1))

;;; minibuffer

(use-package minibuffer
  :ensure nil
  :custom-face
  (completions-annotations ((t :underline nil :inherit (italic shadow))))
  :custom
  ;; (completion-category-overrides nil)
  ;; (completion-category-overrides '((command (styles substring))
  ;;                                  (symbol-help (styles substring))
  ;;                                  (file (styles substring))))
  (completion-styles '(emacs22 partial-completion))
  (completion-pcm-leading-wildcard t)
  (completions-detailed t)
  (completions-group t)
  (read-buffer-completion-ignore-case t)
  :bind
  ("M-i" . completion-at-point)
  (:map minibuffer-local-map
        ("C-u" . kill-whole-line)))

;;; navigation

(use-package recentf
  :ensure nil
  :custom
  (recentf-mode t)
  (recentf-max-saved-items nil)
  (recentf-exclude (list (rx bos "/nix/store"))))

(use-package xref
  :ensure nil
  :custom
  (xref-prompt-for-identifier nil)
  (xref-show-definitions-function #'xref-show-definitions-completing-read))

(use-package isearch
  :ensure nil
  :bind
  ("C-r" . isearch-backward-regexp)
  ("C-s" . isearch-forward-regexp)
  ("M-s" . isearch-forward-symbol)
  (:map isearch-mode-map
        ("ESC" . isearch-exit)))

(use-package imenu
  :ensure nil
  :custom
  (imenu-auto-rescan t)
  (imenu-flatten 'annotation)
  :bind
  ("M-g" . imenu))

(use-package find-func
  :ensure nil
  :custom
  (find-function-mode t)
  :bind
  (:map find-function-mode-map
        ("C-x f" . find-function-other-window)
        ("C-x C-l" . find-library-other-window)))

(use-package savehist
  :ensure nil
  :custom
  (savehist-mode t)
  (savehist-additional-variables '(kill-ring)))

;;; integrations

(use-package tramp
  :ensure nil
  :custom
  (tramp-save-ad-hoc-proxies t)
  (tramp-show-ad-hoc-proxies t))

(use-package eshell
  :ensure nil
  :custom
  (eshell-visual-subcommands '(("bootc" "update")))
  :bind
  (:map project-prefix-map
        ("s" . eshell)))

(use-package esh-mode
  :ensure nil
  :bind
  (:map eshell-mode-map
        ("C-w" . unix-word-rubout)
        ("C-u" . eshell-kill-input)))

(use-package flymake
  :ensure t
  :pin gnu
  :hook
  ((sh-base-mode) . flymake-mode))

(use-package project
  :ensure nil
  :bind
  (:map project-prefix-map
        ("d" . project-dired)))

;;; built-in major modes

(use-package treesit
  :ensure nil
  :defines treesit-language-source-alist
  :init
  (setf (alist-get 'nix treesit-language-source-alist) '("https://github.com/nix-community/tree-sitter-nix.git" "v0.3.0"))
  :custom
  (treesit-auto-install-grammar 'always)
  (treesit-enabled-modes '(bash-ts-mode markdown-ts-mode))
  (treesit-font-lock-level 4))

(use-package dired
  :ensure nil
  :custom
  (dired-listing-switches "-alZ")
  (dired-kill-when-opening-new-dired-buffer t)
  :hook
  (dired-mode . dired-hide-details-mode)
  :bind
  (:map ctl-x-map
        ("d" . dired-jump-other-window))
  (:map dired-mode-map
        ([remap dired-mouse-find-file-other-window] . dired-mouse-find-file)))

(use-package customize
  :ensure nil
  :bind
  (:map help-map
        ("g" . customize-group-other-window)
        ("C-h" . nil)))

(use-package help
  :ensure nil
  :custom
  (help-window-select t)
  :bind
  (:map help-mode-map
        ("p" . help-go-back)))

(use-package info
  :ensure nil
  :bind
  (:map help-map
        ("s" . info-lookup-symbol)))

(use-package conf-mode
  :ensure nil
  :mode
  ((rx "." (or "container" "volume" "service" "pod") eos) . conf-desktop-mode))

(use-package js
  :ensure nil
  :mode ((rx ".conflist" eos) . js-json-mode))

(use-package sh-script
  :ensure nil
  :custom
  (sh-basic-offset 2))

(use-package elisp-mode
  :ensure nil
  :bind
  (:map emacs-lisp-mode-map
        ("C-c C-c" . elisp-eval-region-or-buffer)))

(use-package comint
  :ensure nil
  :custom
  (comint-prompt-read-only t)
  :bind
  (:map comint-mode-map
        ("C-c C-c" . comint-kill-whole-line)
        ("<up>" . comint-previous-input)
        ("<down>" . comint-next-input)))

(use-package dockerfile-ts-mode
  :ensure nil
  :mode ((rx (or "Docker" "Container") "file" (* any) eos)))

(use-package yaml-ts-mode
  :ensure nil
  :mode ((rx "." (or "yaml" "yml") eos)))

;;; built-in minor modes

(use-package pp
  :ensure nil
  :config
  (define-advice pp-display-expression
      (:after (&rest args) readonly)
    (let ((out-buffer-name (cadr args)))
      (with-current-buffer out-buffer-name
        (view-mode))
      (pop-to-buffer out-buffer-name)))
  :bind
  (:map emacs-lisp-mode-map
        ("C-c C-p" . pp-macroexpand-last-sexp)))

(use-package autoinsert
  :ensure nil
  :custom
  (auto-insert-query nil)
  (auto-insert-alist (list (cons "\\.el\\'"
                                 (lambda ()
                                   (setq-local lexical-binding t)
                                   (add-file-local-variable-prop-line 'lexical-binding t)
                                   (goto-char (point-max))))))
  :hook
  (emacs-lisp-mode . auto-insert))

(use-package autorevert
  :ensure nil
  :custom
  (auto-revert-mode-text ""))

;;; third-party completion

(use-package vertico
  :ensure t
  :pin gnu
  :custom
  (vertico-mode t)
  (vertico-count-format nil)
  (vertico-group-format "%s")
  (vertico-multiform-mode t)
  (vertico-multiform-categories '((file (:keymap . vertico-directory-map)))))

(use-package cape
  :ensure t
  :pin gnu
  :init
  (setq-mode-local emacs-lisp-mode
                   completion-at-point-functions '(cape-elisp-symbol t)))

(use-package consult
  :ensure t
  :pin gnu
  :custom
  (consult-async-split-style nil)
  (completion-in-region-function #'consult-completion-in-region)
  (xref-show-xrefs-function #'consult-xref)
  :config
  (defun consult-ripgrep-or-grep ()
    "If ripgrep is available, search with `consult-ripgrep'. Otherwise, search with `consult-grep'."
    (interactive)
    (call-interactively
     (if (executable-find "rg" t)
         #'consult-ripgrep
       #'consult-grep)))
  :bind
  (:map ctl-x-map
        ("C-r" . consult-recent-file))
  (:map project-prefix-map
        ("g" . consult-ripgrep-or-grep)
        ("f" . consult-find))
  (:map help-map
        ("i" . consult-info)))

(use-package embark
  :ensure t
  :pin gnu
  :custom
  (embark-indicators '(embark-minimal-indicator
                       embark-highlight-indicator
                       embark-isearch-highlight-indicator))
  (prefix-help-command #'embark-prefix-help-command)
  :bind
  ("C-." . embark-act)
  ("M-." . embark-act)
  (:map help-map
        ("b" . embark-bindings)))

(use-package embark-consult
  :ensure t
  :pin gnu)

(use-package orderless
  :ensure t
  :demand t
  :custom
  (completion-category-overrides '((command (styles orderless))
                                   (symbol-help (styles orderless)))))

;;; third-party minor modes

(use-package smartparens
  :ensure t
  :pin melpa-stable
  :init
  (require 'smartparens-config)
  :custom
  (sp-echo-match-when-invisible nil)
  (sp-escape-quotes-after-insert nil)
  (sp-highlight-pair-overlay nil)
  (smartparens-global-mode t)
  :config
  (defun lina-sp-mode-hook ()
    (show-paren-local-mode -1)
    (show-smartparens-mode t))
  (defun sp-c-w-dwim ()
    (interactive)
    (if (use-region-p)
        (sp-kill-region (region-beginning) (region-end))
      (sp-backward-kill-sexp current-prefix-arg)))
  :hook
  (smartparens-mode . lina-sp-mode-hook)
  (lisp-data-mode . smartparens-strict-mode)
  :bind
  (:map smartparens-mode-map
        ("C-w" . sp-c-w-dwim)
        ("C-c ." . sp-forward-slurp-sexp)
        ("C-c s" . sp-splice-sexp)
        ("C-c r" . sp-raise-sexp)
        ("M-<left>" . sp-backward-sexp)
        ("M-<right>" . sp-forward-sexp))
  (:repeat-map smartparens-mode-repeat-map
               ("." . sp-forward-slurp-sexp)))

(use-package aggressive-indent
  :ensure t
  :pin gnu
  :hook (lisp-data-mode . aggressive-indent-mode))

(use-package hungry-delete
  :ensure t
  :hook (aggressive-indent-mode . hungry-delete-mode))

;;; third-party integrations

(use-package dumb-jump
  :ensure t
  :init
  (setq-default xref-backend-functions '(dumb-jump-xref-activate))
  (setq-mode-local emacs-lisp-mode
                   xref-backend-functions '(dumb-jump-xref-activate elisp--xref-backend t)))

(use-package gptel
  :pin nongnu
  :custom
  (gptel-log-level 'debug)
  (gptel-include-reasoning t)
  (gptel-model 'gemini-2.5-flash)
  (gptel-default-mode 'markdown-ts-mode)
  :config
  (setopt gptel-backend (gptel-make-gemini "Gemini"
                          :key (gptel-api-key-from-auth-source "generativelanguage.googleapis.com")
                          :stream t)))

(use-package magit
  :ensure t
  :pin nongnu
  :preface
  (setq magit-define-global-key-bindings nil)
  :custom
  (magit-display-buffer-function #'display-buffer)
  :bind
  (:map ctl-x-map
        ("g" . magit-dispatch))
  (:map mode-specific-map
        ("g" . magit-file-dispatch)))

(use-package transient
  :custom
  (transient-display-buffer-action '(display-buffer-at-bottom
                                     (dedicated . t)
                                     (inhibit-same-window . t)))
  :config
  (transient-bind-q-to-quit))

(use-package envrc
  :custom
  (envrc-global-mode t)
  (envrc-none-lighter nil))

(use-package with-editor
  :ensure t
  :hook (eshell-mode . with-editor-export-editor))

;;; third-party major modes
