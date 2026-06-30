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

(add-to-list 'load-path (locate-user-emacs-file "lisp/lina"))

;;;; emacs

(use-package emacs
  :ensure nil
  :preface
  (defconst linux-font '(:family "Iosevka" :height 105))
  :custom
  ((auth-sources '("~/.authinfo"))
   (auto-save-default nil)
   (backward-delete-char-untabify-method 'hungry)
   (bidi-inhibit-bpa t)
   (bidi-paragraph-direction 'left-to-right)
   (column-number-mode t)
   (confirm-kill-processes nil)
   (create-lockfiles nil)
   (cursor-in-non-selected-windows nil)
   (delete-selection-mode t)
   (enable-recursive-minibuffers t)
   (extended-command-suggest-shorter nil)
   (fast-but-imprecise-scrolling t)
   (fill-column 80)
   (garbage-collection-messages t)
   (indent-tabs-mode nil)
   (inhibit-startup-screen t)
   (initial-major-mode 'fundamental-mode)
   (initial-scratch-message nil)
   (kill-do-not-save-duplicates t)
   (kill-region-dwim 'emacs-word)
   (make-backup-files nil)
   (max-redisplay-ticks 1000000)
   (mouse-autoselect-window t)
   (native-comp-async-on-battery-power nil)
   (native-comp-async-report-warnings-errors 'silent)
   (read-extended-command-predicate #'command-completion-default-include-p)
   (read-process-output-max 1048576)
   (redisplay-skip-fontification-on-input t)
   (repeat-mode t)
   (require-final-newline t)
   (ring-bell-function #'ignore)
   (scroll-conservatively 100)
   (show-paren-context-when-offscreen t)
   (suggest-key-bindings nil)
   (tab-always-indent 'complete)
   (tooltip-delay 0.1)
   (trusted-content `(,(locate-user-emacs-file "lisp/lina/")))
   (use-dialog-box nil)
   (use-short-answers t)
   (vc-follow-symlinks t)
   (view-read-only t)
   (warning-minimum-level :emergency)
   (xterm-mouse-mode t)
   (xterm-set-window-title t)
   (auto-revert-mode-text "")
   (register-use-preview nil))
  :custom-face
  (default ((((type x pgtk)) ,linux-font)))
  (fixed-pitch ((((type x pgtk)) ,linux-font)))
  (fixed-pitch-serif ((t :inherit (fixed-pitch))))
  :bind (("M-u" . ignore)
         ("M-;" . comment-line)
         ("C-l" . redraw-display)
         ;; ("C-x m" . push-point-to-register)
         ("C-x j" . register-to-point)
         ("C-x C-g" . ignore)
         ("M-<up>" . backward-up-list)
         ("M-<down>" . down-list)
         ("M-<left>" . backward-sexp)
         ("M-<right>" . forward-sexp)
         ("C-k" . kill-whole-line)
         ("C-z" . undo)
         ("C-S-z" . undo-redo)
         ("M-," . pop-to-mark-command)
         ("C-," . pop-global-mark)
         ("C-x x" . revert-buffer-quick)
         ("C-x C-x" . revert-buffer-quick)))

(use-package server
  :ensure nil
  :autoload server-running-p
  :hook
  (after-init-hook . (lambda ()
                       (unless (server-running-p)
                         (server-start)))))

(require-theme 'modus-themes)
(add-hook 'after-init-hook (lambda ()
                             (load-theme 'modus-operandi t)))

(use-package minibuffer
  :ensure nil
  :custom
  (completion-ignore-case t)
  (completion-pcm-leading-wildcard t)
  (completion-show-help nil)
  (completions-detailed t)
  (completions-group t)
  (completions-max-height 6)
  (minibuffer-nonselected-mode nil)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  :custom-face
  (completions-annotations ((t :underline nil :inherit (italic shadow))))
  :bind
  ("M-i" . completion-at-point)
  (:map minibuffer-local-map
        ("C-u" . kill-whole-line)))

;;; movec

(use-package vertico
  :ensure t
  :pin gnu
  :custom
  (vertico-mode t)
  (vertico-count-format nil)
  (vertico-group-format "%s")
  (vertico-multiform-mode t)
  (vertico-multiform-categories '((file (:keymap . vertico-directory-map))))
  (vertico-multiform-commands '((magit-clone (vertico-preselect . prompt)))))

(use-package cape
  :ensure t
  :pin gnu
  :defines emacs-lisp-mode autoconf-mode
  :custom
  (cape-elisp-symbol-wrapper nil)
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
  :autoload consult-ripgrep consult-grep
  :custom
  (consult-async-split-style nil)
  (consult-ripgrep-args "rg \
--null \
--line-buffered \
--color=never \
--max-columns=1000 \
--path-separator=/ \
--smart-case \
--no-heading \
--with-filename \
--line-number \
--search-zip \
--glob=!TAGS")
  (consult-preview-key nil)
  (completion-in-region-function #'consult-completion-in-region)
  (xref-show-xrefs-function #'consult-xref)
  :config
  (defun consult-ripgrep-or-grep (&optional dir initial)
    "If ripgrep is available, search with `consult-ripgrep'. Otherwise, search
with `consult-grep'."
    (interactive "P")
    (funcall (if (executable-find "rg" t)
                 #'consult-ripgrep
               #'consult-grep)
             dir initial))
  :bind
  ("M-g" . consult-imenu)
  (:map ctl-x-map
        ("b" . consult-buffer))
  (:map ctl-x-r-map
        ("SPC" . consult-register-store)
        ("j" . consult-register-load)
        ("b" . consult-bookmark))
  (:map project-prefix-map
        ("g" . consult-ripgrep)
        ("f" . consult-find))
  (:map help-map
        ("i" . consult-info)))

(use-package embark
  :defines embark-general-map embark-target-finders
  :ensure t
  :pin gnu
  :custom
  (embark-cycle-key "TAB")
  (embark-indicators '(embark-minimal-indicator
                       embark-highlight-indicator
                       embark-isearch-highlight-indicator))
  (prefix-help-command #'embark-prefix-help-command)
  :config
  (delete 'embark-target-flymake-at-point embark-target-finders)
  (defun embark-isearch-symbol-forward ()
    "`embark-isearch-forward' but `isearch-forward-symbol'."
    (interactive)
    (isearch-mode t nil nil nil 'isearch-symbol-regexp)
    (isearch-edit-string))
  :bind
  (("C-." . embark-act)
   ("M-." . embark-dwim)
   ("C-x ." . embark-act)
   (:map help-map
         ("b" . embark-bindings))
   (:map embark-general-map
         ("C-s" . embark-isearch-symbol-forward))))

(use-package embark-consult
  :ensure t
  :pin gnu)

(use-package orderless
  :ensure t
  :pin gnu
  :demand t
  :custom
  (completion-styles '(emacs22 partial-completion orderless))
  (completion-category-overrides
   `((multi-category (styles substring))
     (buffer (styles substring))
     ,@(mapcar (lambda (cat)
                 (list cat '(styles orderless)))
               '(command symbol function variable symbol-help))))
  (orderless-component-separator "[- ]")
  :config
  (setq-mode-local emacs-lisp-mode completion-styles '(orderless)))

(use-package corfu
  :defines corfu-map
  :ensure t
  :if (or (display-graphic-p)
          (>= emacs-major-version 31))
  :custom
  (corfu-cycle t)
  (corfu-quit-at-boundary t)
  (corfu-quit-no-match nil)
  (corfu-preselect 'first)
  (global-corfu-minibuffer nil)
  (global-corfu-mode t)
  (global-corfu-modes '((not comint-mode eshell-mode) t))
  :bind (:map corfu-map
              ("TAB" . corfu-next)
              ("<backtab>" . corfu-previous)))

(use-package marginalia
  :ensure t
  :custom (marginalia-mode t))

;;;; help

(use-package customize
  :ensure nil
  :bind
  (:map help-map
        ("g" . customize-group-other-window)
        ("u" . customize-variable-other-window)))

(use-package help
  :ensure nil
  :custom
  (help-window-select t)
  :bind
  (:map help-map
        ("m" . describe-keymap)
        ("C-h" . nil)
        ("C-g" . help-quit))
  (:map help-mode-map
        ("," . help-go-back)
        ("p" . help-go-back)))

(use-package info
  :ensure nil
  :defines Info-mode-map
  :bind
  (:map Info-mode-map
        ("R" . info-display-manual)
        ("s" . consult-info))
  (:map help-map
        ("s" . info-lookup-symbol)))

(use-package man
  :ensure nil
  :functions Man-notify-when-ready@display-buffer
  :config
  (define-advice Man-notify-when-ready (:override (buffer) display-buffer)
    "Call `display-buffer' with BUFFER and action (category . man)."
    (display-buffer buffer '(nil (category . man)))))

;;; built-in minor modes

;;;; built-in global minor modes

(use-package recentf
  :ensure nil
  :custom
  (recentf-mode t)
  (recentf-max-saved-items nil)
  (recentf-exclude `(,(rx bos "/nix/store/" (* nonl)))))

(use-package eldoc
  :ensure nil
  :custom
  (eldoc-minor-mode-string nil)
  (eldoc-echo-area-use-multiline-p nil))

(use-package display-fill-column-indicator
  :ensure nil
  :custom
  (display-fill-column-indicator-character nil)
  (global-display-fill-column-indicator-mode t)
  (global-display-fill-column-indicator-modes '(prog-mode))
  :custom-face
  (fill-column-indicator ((t :foreground ,"grey" :background unspecified))))

(use-package savehist
  :ensure nil
  :custom
  (savehist-mode t)
  (savehist-additional-variables '(kill-ring)))

(use-package saveplace
  :ensure nil
  :custom
  (save-place-mode t))

;;;; built-in local minor modes

(use-package flymake
  :ensure nil
  :config
  (unless (or (server-running-p)
              (display-graphic-p))
    (setq-default flymake-show-diagnostics-at-end-of-line 'short))
  :bind
  (:map project-prefix-map
        ("n" . flymake-show-project-diagnostics))
  (:map flymake-mode-map
        ("C-x n" . flymake-show-buffer-diagnostics)))

;;;;; eglot

(require 'eglot)
(setopt eglot-stay-out-of '(flymake)
        eglot-send-changes-idle-time 1
        eglot-server-programs
        '(((python-mode python-ts-mode) "ty" "server")
          (haskell-mode "haskell-language-server-wrapper" "--lsp")))
(add-hook 'eglot-managed-mode-hook
          (defun lina/eglot-hook ()
            (eglot-inlay-hints-mode (if (member major-mode
                                                '(python-mode python-ts-mode))
                                        -1
                                      t))
            (add-hook 'flymake-diagnostic-functions
                      #'eglot-flymake-backend nil t)
            (flymake-mode t)))

;;; built-in commands

(use-package project
  :ensure nil
  :bind
  (:map project-prefix-map
        ("d" . project-dired)
        ("s" . project-eshell)))

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
  (:map isearch-mode-map
        ("ESC" . isearch-exit)
        ("TAB" . isearch-toggle-symbol)
        ("<left>" . isearch-edit-string)
        ("<right>" . isearch-edit-string)))

(use-package find-func
  :ensure nil
  :custom
  (find-function-mode t)
  (find-function-mode-lower-precedence t))

(use-package ispell
  :ensure nil
  :custom
  (ispell-dictionary "en_GB"))

(use-package browse-url
  :ensure nil
  :custom
  (browse-url-handlers `((,(rx ".pdf" eos)
                          .
                          ,(cl-case system-type
                             (gnu/linux #'browse-url-xdg-open))))))

(use-package windmove
  :ensure nil
  :custom (windmove-mode t)
  :bind (:map windmove-mode-map
              ("C-S-<left>" . windmove-swap-states-left)
              ("C-S-<right>" . windmove-swap-states-right)
              ("C-S-<up>" . windmove-swap-states-up)
              ("C-S-<down>" . windmove-swap-states-down)))

;;; built-in externals

(use-package tramp
  :functions tramp-recentf-cleanup tramp-recentf-cleanup-all tramp-enable-method
  :ensure nil
  :demand t
  :custom
  (tramp-show-ad-hoc-proxies t)
  :config
  (advice-add #'tramp-recentf-cleanup :override #'ignore)
  (advice-add #'tramp-recentf-cleanup-all :override #'ignore)
  (tramp-enable-method 'podman)
  (tramp-enable-method 'distrobox))

(use-package compile
  :ensure nil
  :custom
  ((compilation-scroll-output 'first-error)
   (compilation-ask-about-save nil)))

(use-package vc-hooks
  :ensure nil
  :hook (after-init-hook . (lambda ()
                             (setq-default vc-handled-backends '(Git)))))

;;;; shells

(use-package shell
  :ensure nil
  :custom
  ((shell-kill-buffer-on-exit t)
   (explicit-shell-file-name (or
                              (let ((zsh (executable-find "zsh")))
                                (and (file-exists-p "~/.zshrc")
                                     zsh))
                              "/bin/bash"))))

(use-package term
  :ensure nil
  :config
  (defun lina/term-hook ()
    (face-remap-set-base 'default '(:family "JetBrains Mono NL")))
  :bind (:map term-raw-map
              ("C-x" . nil)
              ("C-h" . nil)
              ("M-x" . nil)))

(use-package comint
  :ensure nil
  :autoload comint-skip-input
  :custom
  ((comint-prompt-read-only t)
   (comint-scroll-to-bottom-on-input t)
   (comint-move-point-for-output t))
  :bind (:map comint-mode-map
              ("<up>" . comint-previous-input)
              ("<down>" . comint-next-input)
              ("C-u" . comint-kill-input)))

;;;;; eshell

(use-package esh-mode
  :functions eshell-reset
  :ensure nil
  :config
  (defun lina/eshell-hook ()
    (electric-pair-local-mode -1))
  (when (fboundp 'unix-word-rubout)
    (bind-key "C-w" #'unix-word-rubout eshell-mode-map))
  :hook (eshell-mode-hook . lina/eshell-hook)
  :bind (:map eshell-mode-map
              ("C-u" . eshell-kill-input)
              ("C-d" . eshell-send-eof-to-process)))

(use-package eshell
  :functions eshell/cd
  :ensure nil
  :custom
  (eshell-scroll-to-bottom-on-input t)
  (eshell-visual-subcommands '(("sudo" "bootc" "update")
                               ("sudo" "dnf" "install")))
  :config
  (defun lina/eshell-in-buffer-directory ()
    (interactive)
    (let ((bufdir default-directory))
      (with-current-buffer (eshell)
        (unless (string= bufdir default-directory)
          (eshell/cd `(,bufdir))
          (eshell-reset)))))
  (define-advice eshell (:around (fun &rest _) display-buffer)
    (let ((display-buffer--same-window-action '(())))
      (call-interactively fun))))

;;; third-party integrations

(use-package dumb-jump
  :ensure t
  :custom
  ((dumb-jump-prefer-searcher 'rg))
  :init
  (setq-default xref-backend-functions '(dumb-jump-xref-activate)))

(use-package magit
  :functions magit-clone-read-args@vertico-preselect
  :pin nongnu
  :preface
  (setq magit-define-global-key-bindings nil)
  :custom
  (magit-display-buffer-function #'display-buffer)
  (magit-commit-show-diff nil)
  :config
  (define-advice magit-clone-read-args (:around (function) vertico-preselect)
    (let (vertico-multiform-categories)
      (push '(vertico-preselect . prompt)
            (alist-get 'directory vertico-multiform-categories))
      (funcall function)))
  :bind
  (:map ctl-x-map
        ("g" . magit-dispatch))
  (:map mode-specific-map
        ("g" . magit-file-dispatch)))

(use-package transient
  :functions transient-bind-q-to-quit
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

(use-package delight
  :ensure t)

(use-package ghostel
  :pin melpa
  :custom
  ((ghostel-shell (or (executable-find "zsh")
                      "/bin/sh"))
   (ghostel-term "xterm-256color")))

(use-package apheleia
  :defines apheleia-mode-alist
  :ensure t
  :custom
  (apheleia-formatters
   '((ruff "uvx"
           "--quiet"
           "ruff"
           "format"
           "--silent"
           (apheleia-formatters-fill-column "--line-length")
           "--stdin-filename"
           filepath
           "-")
     (ruff-isort "uvx"
                 "--quiet"
                 "ruff"
                 "check"
                 "-n"
                 "--select"
                 "I"
                 "--fix"
                 "--fix-only"
                 "--stdin-filename"
                 filepath
                 "-")
     (tex-fmt "tex-fmt"
              "--stdin"
              "-v")))
  (apheleia-mode-alist
   '((python-base-mode . (ruff ruff-isort))
     (tex-mode . tex-fmt))))

;;; third-party minor modes

(use-package aggressive-indent
  :ensure t
  :pin gnu
  :hook (lisp-data-mode-hook . aggressive-indent-mode))

(use-package gcmh
  :ensure t
  :delight gcmh-mode
  :hook (after-init-hook . gcmh-mode))

;;; built-in virtual major modes

(use-package dired
  :ensure nil
  :custom
  (delete-by-moving-to-trash t)
  (dired-auto-revert-buffer t)
  (dired-clean-confirm-killing-deleted-buffers nil)
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-listing-switches "-alZ")
  (dired-recursive-deletes 'always)
  :hook
  (dired-mode-hook . dired-hide-details-mode)
  :bind
  (:map ctl-x-map
        ("d" . dired-jump))
  (:map dired-mode-map
        ([remap dired-mouse-find-file-other-window]
         . dired-mouse-find-file)))

(use-package image-mode
  :ensure nil
  :bind (:map image-mode-map
              ([remap revert-buffer] . revert-buffer-quick)))

;;; built-in language major modes

(use-package prog-mode
  :ensure nil
  :init
  (defun lina/c-w-dwim ()
    (interactive)
    (call-interactively (if (use-region-p) #'kill-region #'backward-kill-sexp)))
  :config
  (defun lina/prog-mode-hook ()
    (goto-address-prog-mode t)
    (when (fboundp 'delete-trailing-whitespace-mode)
      (delete-trailing-whitespace-mode t)))
  :hook (prog-mode-hook . lina/prog-mode-hook)
  :bind (:map prog-mode-map
              ("C-w" . lina/c-w-dwim)))

(use-package treesit
  :ensure nil
  :defines treesit-language-source-alist
  :init
  (setf (alist-get 'nix treesit-language-source-alist)
        '("https://github.com/nix-community/tree-sitter-nix.git"
          "v0.3.0"))
  :custom
  (treesit-auto-install-grammar 'always)
  (treesit-enabled-modes '(bash-ts-mode
                           c-ts-mode
                           json-ts-mode
                           typescript-ts-mode
                           tsx-ts-mode
                           python-ts-mode))
  (treesit-font-lock-level 4))

(use-package conf-mode
  :ensure nil
  :mode
  (((rx "." (or "container" "volume" "service" "pod") eos) . conf-desktop-mode)
   ((rx "/isyncrc" eos) . conf-space-mode)
   ((rx ".ovpn" eos) . conf-space-mode)
   ((rx "/" (or "sysusers.d" "tmpfiles.d") "/" (+ nonl) ".conf" eos)
    .
    conf-space-mode)))

(use-package js
  :ensure nil
  :custom (js-indent-level 2)
  :mode ((rx ".conflist" eos) . js-json-mode))

(use-package sh-script
  :ensure nil
  :custom
  (sh-basic-offset 2)
  :config
  (defun lina/shell-mode-hook ()
    (flymake-mode t))
  :hook (sh-base-mode-hook . lina/shell-mode-hook)
  :mode ((rx "/.env" (opt ".local")) . sh-mode))

(use-package dockerfile-ts-mode
  :ensure nil
  :defines dockerfile-ts-mode
  :config
  (setq-mode-local dockerfile-ts-mode indent-line-function
                   #'indent-relative-first-indent-point)
  :mode ((rx (or "Docker" "Container") "file" (* nonl) eos)))

(use-package python
  :ensure nil
  :custom
  ((python-flymake-command '("ruff"
                             "check"
                             "--quiet"
                             "--output-format=concise"
                             "--stdin-filename=stdin"))
   (python-indent-guess-indent-offset-verbose nil)
   (python-shell-dedicated 'buffer))
  :config
  (defun lina/python-mode-hook ()
    (setq-local fill-column 79
                tab-always-indent t)
    (flymake-mode t)
    (display-line-numbers-mode t)
    (when (fboundp 'apheleia-mode)
      (setq-local apheleia-formatters-respect-fill-column t)
      (apheleia-mode t)))
  :hook (python-base-mode-hook . lina/python-mode-hook))

(use-package tex-mode
  :ensure nil
  :defines latex-mode
  :config
  (defun lina/tex-hook ()
    (setq-local compile-command "latexmk \
-file-line-error \
-halt-on-error \
-interaction=nonstopmode \
-synctex=1")
    (visual-line-mode t)
    (when (fboundp 'hungry-delete-mode)
      (hungry-delete-mode t))
    (when (fboundp 'smartparens-mode)
      (smartparens-mode t))
    (when (and (fboundp 'apheleia-mode)
               (bound-and-true-p apheleia-mode-alist)
               (assq 'tex-mode apheleia-mode-alist))
      (apheleia-mode t)))
  :hook (tex-mode-hook . lina/tex-hook)
  :bind (:map latex-mode-map
              ("C-c C-c" . recompile)))

(use-package backtrace
  :ensure nil
  :config
  (defun lina/backtrace-mode-hook ()
    (setq-local truncate-lines nil))
  :hook (backtrace-mode-hook . lina/backtrace-mode-hook))

;;; third-party major modes

(use-package nix-ts-mode
  :init
  (setf (alist-get 'nix-mode major-mode-remap-alist) 'nix-ts-mode)
  :mode "\\.nix\\'")

(autoload 'inheritenv-add-advice "inheritenv"
  "Advise function FUNC with `inheritenv-apply'.
This will ensure that any buffers (including temporary buffers)
created by FUNC will inherit the caller's environment." nil 'macro)

(use-package kubed
  :vc (:url "https://git.sr.ht/~eshel/kubed" :rev "v0.7.0")
  :bind
  ("C-c k" . kubed-transient))

(use-package yaml-mode
  :ensure t
  :mode ((rx "." (or "yaml" "yml") eos)))

;;; other files

(load "lina-window")
(load "lina-smartparens")
(load "lina-elisp")
(load "lina-llm")
(load "lina-mail")
;; (load "lina-puni")
