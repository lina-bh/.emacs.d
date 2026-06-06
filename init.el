;; -*- lexical-binding: t; -*-
(autoload 'setq-mode-local "mode-local")

;;; initialisation & general behaviour
(setq-default custom-file (locate-user-emacs-file "custom.el")
              use-package-always-defer t
              use-package-enable-imenu-support t
              use-package-hook-name-suffix nil
              package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                                 ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                                 ("melpa"
                                  . "https://melpa.org/packages/")
                                 ("melpa-stable"
                                  . "https://stable.melpa.org/packages/"))
              package-archive-priorities '(("gnu" . 3)
                                           ("nongnu" . 2)
                                           ("melpa-stable" . 1)))
(load custom-file t)
(package-initialize)
;; (package-upgrade-all)
(add-to-list 'load-path (locate-user-emacs-file "lisp/lina"))

(use-package emacs
  :ensure nil
  :preface
  (defconst linux-font '(:family "Iosevka Nerd Font" :height 105))
  :custom
  (auth-sources '("~/.authinfo"))
  (auto-save-default nil)
  (create-lockfiles nil)
  (delete-selection-mode t)
  (electric-pair-mode t)
  (enable-recursive-minibuffers t)
  (fill-column 80)
  (garbage-collection-messages t)
  (find-function-mode t)
  (indent-tabs-mode nil)
  (inhibit-startup-screen t)
  (initial-scratch-message nil)
  (initial-major-mode 'fundamental-mode)
  (make-backup-files nil)
  (mouse-autoselect-window t)
  (repeat-mode t)
  (require-final-newline t)
  (savehist-mode t)
  (save-place-mode t)
  (save-interprogram-paste-before-kill t)
  (tab-bar-mode t)
  (use-short-answers t)
  (vc-follow-symlinks t)
  (view-read-only t)
  (warning-minimum-level :emergency)
  (xterm-set-window-title t)
  (xterm-mouse-mode t)
  :custom-face
  (default ((((type x pgtk)) ,linux-font)))
  (fixed-pitch ((((type x pgtk)) ,linux-font)))
  (fixed-pitch-serif ((t :inherit (fixed-pitch))))
  :hook
  (prog-mode-hook . delete-trailing-whitespace-mode)
  :bind
  ("M-u" . ignore)
  ("C-z" . undo)
  ("M-," . pop-to-mark-command)
  ("C-," . nil)
  ("C-." . nil)
  ("M-;" . comment-line)
  ("M-<left>" . backward-sexp)
  ("M-<right>" . forward-sexp)
  ("M-<up>" . backward-up-list)
  ("M-<down>" . down-list)
  ("C-l" . redraw-display)
  ("C-," . pop-global-mark)
  (:map ctl-x-map
        ("x" . revert-buffer-quick)
        ("C-x" . revert-buffer-quick)
        ("," . pop-global-mark)
        ("m" . push-point-to-register)
        ("j" . register-to-point))
  (:map image-mode-map
        ([remap revert-buffer] . revert-buffer-quick)))

(use-package server
  :ensure nil
  :autoload server-running-p
  :init
  (defun lina/after-init-start-server ()
    (unless (server-running-p)
      (server-start)))
  :hook
  (after-init-hook . lina/after-init-start-server))

;;; look & feel

(use-package window
  :ensure nil
  :custom
  (display-buffer-base-action '((display-buffer-reuse-window
                                 display-buffer-use-least-recent-window)))
  (display-buffer-alist
   `(((derived-mode . magit-revision-mode)
      display-buffer-reuse-mode-window
      (mode . magit-log-mode))
     ("\\*Completions"
      (display-buffer-reuse-window
       display-buffer-at-bottom))
     ((derived-mode . calc-mode)
      display-buffer-at-bottom)
     ((or
       (category . comint)
       (category . warning)
       (derived-mode . flymake-diagnostics-buffer-mode)
       (derived-mode . help-mode)
       (derived-mode . term-mode)
       (derived-mode . comint-mode)
       ,(rx bos
            "*"
            (or
             "eshell"
             "trace-output"
             "eldoc"
             (and (* any) "REPL")
             "Warnings")))
      display-buffer-in-side-window
      (window-height . 12)
      (slot . 0))
     ((or
       ,(rx bos "*" (or "Customize" "Man")))
      (display-buffer-reuse-mode-window))
     (,(rx bos "*Pp")
      (display-buffer-below-selected))))
  (switch-to-buffer-in-dedicated-window 'pop)
  (switch-to-buffer-obey-display-actions t)
  (split-height-threshold nil)
  (split-window-preferred-direction 'horizontal)
  :config
  (defun split-window-right-and-select (&rest args)
    (interactive)
    (select-window (apply #'split-window-right args)))
  :bind
  ("C-x 5" . make-frame-command)
  (:map ctl-x-map
        ("3" . split-window-right-and-select)
        ("q" . quit-window)))

(use-package tooltip
  :ensure nil
  :custom
  (tooltip-delay 0.1))

(use-package modus-themes
  :ensure nil
  :demand t
  :load-path (lambda ()
               (file-name-concat data-directory "themes"))
  :config
  (defun lina/load-theme ()
    (load-theme 'modus-operandi t))
  :hook (after-init-hook . lina/load-theme))

(use-package display-fill-column-indicator
  :ensure nil
  :custom
  (display-fill-column-indicator-character ?\u2595)
  (global-display-fill-column-indicator-mode t)
  (global-display-fill-column-indicator-modes
   '((not special-mode text-mode racket-repl-mode comint-mode) t))
  :custom-face
  (fill-column-indicator ((t :background nil))))

;;; minibuffer

(use-package minibuffer
  :ensure nil
  :custom-face
  (completions-annotations ((t :underline nil :inherit (italic shadow))))
  :custom
  (completion-ignore-case t)
  (completion-pcm-leading-wildcard t)
  (completion-show-help nil)
  (completion-styles '(emacs22 partial-completion))
  (completions-detailed t)
  (completions-group t)
  (completions-max-height 6)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
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
  (recentf-exclude (list (rx bos "/nix/store" (* any)))))

(use-package xref
  :ensure nil
  :custom
  (xref-prompt-for-identifier nil)
  (xref-show-definitions-function
   #'xref-show-definitions-completing-read))

(use-package isearch
  :ensure nil
  :bind
  ("C-r" . isearch-backward-regexp)
  ("C-s" . isearch-forward-regexp)
  ("M-s" . isearch-forward-symbol)
  (:map isearch-mode-map
        ("ESC" . isearch-exit)))

(use-package find-func
  :ensure nil
  :custom
  (find-function-mode t)
  (find-function-mode-lower-precedence t))

(use-package savehist
  :ensure nil
  :custom
  (savehist-mode t)
  (savehist-additional-variables '(kill-ring)))

;;; integrations

(use-package tramp
  :ensure nil
  :demand t
  :custom
  (tramp-show-ad-hoc-proxies t)
  :config
  (advice-add #'tramp-recentf-cleanup :override #'ignore)
  (advice-add #'tramp-recentf-cleanup-all :override #'ignore)
  (tramp-enable-method 'podman)
  (tramp-enable-method 'distrobox))

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
  :ensure nil
  :custom
  (flymake-show-diagnostics-at-end-of-line t)
  :hook
  ((sh-base-mode-hook python-mode-hook) . flymake-mode)
  :bind
  (:map project-prefix-map
        ("n" . flymake-show-project-diagnostics))
  (:map flymake-mode-map
        ("C-x n" . flymake-show-buffer-diagnostics)))

(use-package project
  :ensure nil
  :bind
  (:map project-prefix-map
        ("d" . project-dired)))

(require 'eglot)
(setopt eglot-stay-out-of '(flymake))
(add-hook 'eglot-managed-mode-hook
          (defun lina/eglot-hook ()
            (eglot-inlay-hints-mode
             (if (member major-mode '(python-mode python-ts-mode))
                 -1
               t))
            (add-hook 'flymake-diagnostic-functions
                      #'eglot-flymake-backend nil t)
            (flymake-mode t)))

;;; built-in major modes

(use-package treesit
  :ensure nil
  :defines treesit-language-source-alist
  :init
  (setf (alist-get 'nix treesit-language-source-alist)
        '("https://github.com/nix-community/tree-sitter-nix.git"
          "v0.3.0"))
  :custom
  (treesit-auto-install-grammar 'always)
  (treesit-enabled-modes '(c-ts-mode
                           bash-ts-mode
                           markdown-ts-mode))
  (treesit-font-lock-level 4))

(use-package dired
  :ensure nil
  :custom
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-listing-switches "-alZ")
  (dired-auto-revert-buffer t)
  :hook
  (dired-mode-hook . dired-hide-details-mode)
  :bind
  (:map ctl-x-map
        ("d" . dired-jump-other-window))
  (:map dired-mode-map
        ([remap dired-mouse-find-file-other-window]
         . dired-mouse-find-file)))

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
        ("," . help-go-back)
        ("p" . help-go-back)))

(use-package info
  :ensure nil
  :bind
  (:map help-map
        ("s" . info-lookup-symbol)))

(use-package conf-mode
  :ensure nil
  :mode
  ((rx "." (or "container" "volume" "service" "pod") eos)
   . conf-desktop-mode))

(use-package js
  :ensure nil
  :custom
  (js-indent-level 2)
  :mode ((rx ".conflist" eos) . js-json-mode))

(use-package sh-script
  :ensure nil
  :custom
  (sh-basic-offset 2))

(use-package elisp-mode
  :ensure nil
  :config
  (defun lina/elisp-hook ()
    (when (and (buffer-file-name)
               (file-in-directory-p (buffer-file-name)
                                    package-user-dir))
      (view-mode)))
  :hook (emacs-lisp-mode-hook . lina/elisp-hook)
  :bind
  (:map emacs-lisp-mode-map
        ("C-c C-c" . elisp-eval-region-or-buffer)))

(use-package comint
  :ensure nil
  :custom
  (comint-prompt-read-only t)
  :bind
  (:map comint-mode-map
        ("<up>" . comint-previous-input)
        ("<down>" . comint-next-input)))

(use-package ielm
  :ensure nil
  :config
  (defun lina/ielm-interrupt ()
    (interactive)
    (comint-skip-input)
    (ielm-return))
  :bind (:map inferior-emacs-lisp-mode-map ("C-c C-c"
                                            . lina/ielm-interrupt)))

(use-package dockerfile-ts-mode
  :ensure nil
  :mode ((rx (or "Docker" "Container") "file" (* any) eos)))

(use-package yaml-ts-mode
  :ensure nil
  :mode ((rx "." (or "yaml" "yml") eos)))

(use-package python
  :ensure nil
  :custom
  (python-flymake-command '("ruff"
                            "check"
                            "--quiet"
                            "--output-format=concise"
                            "--stdin-filename=stdin"))
  (python-indent-guess-indent-offset-verbose nil)
  :config
  (defun lina/python-hook ()
    (set-fill-column 80))
  :hook (python-mode-hook . lina/python-hook))

;;; built-in minor modes

(use-package pp
  :ensure nil
  :config
  (define-advice pp-display-expression
      (:after (_expression out-buffer-name &optional _lisp) readonly)
    (with-current-buffer out-buffer-name
      (view-mode))
    (pop-to-buffer out-buffer-name))
  :bind
  ("M-;" . pp-eval-expression)
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
  (emacs-lisp-mode-hook . auto-insert))

(use-package autorevert
  :ensure nil
  :custom
  (auto-revert-mode-text ""))

(use-package eldoc
  :ensure nil
  :custom
  (eldoc-minor-mode-string nil)
  (eldoc-echo-area-use-multiline-p nil))

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
                   completion-at-point-functions '(cape-elisp-symbol t))
  (setq-mode-local autoconf-mode
                   completion-at-point-functions '(cape-dabbrev t))
  :bind
  ("M-/" . cape-dabbrev))

(use-package consult
  :ensure t
  :pin gnu
  :custom
  (consult-async-split-style nil)
  (xref-show-xrefs-function #'consult-xref)
  :config
  (defun consult-ripgrep-or-grep ()
    "If ripgrep is available, search with `consult-ripgrep'. Otherwise, search with `consult-grep'."
    (interactive)
    (call-interactively
     (if (executable-find "rg" t)
         #'consult-ripgrep
       #'consult-grep)))
  (defun lina/consult-minibuffer-completion-hook ()
    (setq-local completion-in-region-function
                #'consult-completion-in-region))
  :hook (minibuffer-mode-hook
         . lina/consult-minibuffer-completion-hook)
  :bind
  ("M-g" . consult-imenu)
  (:map ctl-x-map
        ("C-r" . consult-recent-file)
        ("r" . consult-bookmark))
  (:map project-prefix-map
        ("g" . consult-ripgrep-or-grep)
        ("f" . consult-find))
  (:map help-map
        ("i" . consult-info)))

(use-package embark
  :ensure t
  :pin gnu
  :custom
  (embark-cycle-key "TAB")
  (embark-indicators '(embark-minimal-indicator
                       embark-highlight-indicator
                       embark-isearch-highlight-indicator))
  (prefix-help-command #'embark-prefix-help-command)
  :bind
  ("C-." . embark-act)
  ("M-." . embark-dwim)
  (:map ctl-x-map
        ("." . embark-act))
  (:map help-map
        ("b" . embark-bindings)))

(use-package embark-consult
  :ensure t
  :pin gnu)

(use-package orderless
  :ensure t
  :pin gnu
  :demand t
  :custom
  (completion-category-overrides '((command (styles orderless))
                                   (symbol-help (styles orderless))))
  (orderless-component-separator "[- ]"))

(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)
  (corfu-quit-at-boundary nil)
  (corfu-quit-no-match nil)
  (global-corfu-minibuffer nil)
  (global-corfu-mode t)
  (global-corfu-modes '((not comint-mode eshell-mode) t))
  :bind (:map corfu-map
              ("TAB" . corfu-next)
              ("<backtab>" . corfu-previous)))

;;; third-party minor modes

(require 'lina-smartparens)

(use-package aggressive-indent
  :ensure t
  :pin gnu
  :hook (lisp-data-mode-hook . aggressive-indent-mode))

(use-package gcmh
  :ensure t
  :delight gcmh-mode
  :hook (after-init-hook . gcmh-mode))

;;; third-party integrations

(use-package dumb-jump
  :ensure t
  :init
  (setq-default xref-backend-functions '(dumb-jump-xref-activate))
  (setq-mode-local emacs-lisp-mode
                   xref-backend-functions
                   '(dumb-jump-xref-activate elisp--xref-backend t)))

(use-package gptel
  :pin nongnu
  :custom
  (gptel-log-level 'debug)
  (gptel-model 'gemini-2.5-flash)
  (gptel-default-mode 'markdown-ts-mode)
  (gptel-directives '((default . "\
Assume the following:
* Respond as a computer program to which language is the interface.
* Do not respond conversationally, but concisely.
* Suggest methods and symbols, and sparingly example code blocks.
* Do not rewrite files and return them.")))
  :config
  (defun lina/gptel-hook ()
    (local-set-key (kbd "C-c C-c") #'gptel-send))
  (setopt gptel-backend (gptel-make-gemini "Gemini"
                          :key (gptel-api-key-from-auth-source "generativelanguage.googleapis.com")
                          :stream t))

  :hook (gptel-mode-hook . lina/gptel-hook))

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
  :pin nongnu
  :hook (eshell-mode-hook . with-editor-export-editor))

(use-package ruff-format
  :hook (python-mode-hook . ruff-format-on-save-mode))

(use-package delight
  :ensure t)

;;; third-party major modes

(use-package nix-ts-mode
  :init
  (setf (alist-get 'nix-mode major-mode-remap-alist) 'nix-ts-mode)
  :mode "\\.nix\\'")

(use-package racket-mode
  :custom
  (racket-xp-eldoc-level 'complete)
  :hook (racket-mode-hook . racket-xp-mode)
  :mode "\\.rkt\\'")

(use-package inf-clojure
  :custom
  (inf-clojure-custom-repl-type 'clojure)
  :custom
  (defun lina/inf-clojure-eval-last-sexp-and-go ()
    (interactive)
    (inf-clojure-eval-last-sexp t))
  :hook (clojure-ts-mode-hook . inf-clojure-minor-mode)
  :bind (:map inf-clojure-minor-mode-map
              ("C-c C-p" . inf-clojure-switch-to-repl)
              ([remap inf-clojure-eval-last-sexp]
               . lina/inf-clojure-eval-last-sexp-and-go)))
