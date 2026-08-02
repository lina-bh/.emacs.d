;; -*- lexical-binding: t; -*-
(require 'cl-lib)

(autoload 'setq-mode-local "mode-local")

;;; belated init

(unless (>= emacs-major-version 31)
  (defvar user-lisp-directory (locate-user-emacs-file "user-lisp/"))
  (cl-pushnew user-lisp-directory load-path)
  (let ((autoload-file (expand-file-name ".user-lisp-autoloads.el"
                                         user-lisp-directory)))
    (loaddefs-generate (list user-lisp-directory)
                       autoload-file)
    (load autoload-file)))
(add-to-list 'load-path (locate-user-emacs-file "lisp/lina"))

;;;; use-package

(setq-default use-package-always-defer t
              use-package-enable-imenu-support t
              use-package-hook-name-suffix nil
              use-package-check-before-init t)

;;;; package.el

(setq-default package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                                 ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                                 ("melpa"
                                  . "https://melpa.org/packages/")
                                 ("melpa-stable"
                                  . "https://stable.melpa.org/packages/"))
              package-archive-priorities '(("gnu" . 3)
                                           ("nongnu" . 2)
                                           ("melpa-stable" . 1))
              package-pinned-packages '((smartparens . "melpa-stable")
                                        (ghostel . "melpa")))
(package-initialize)

(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file t)

;;; emacs
(use-package emacs
  :ensure nil
  :custom
  ((auto-insert-query nil)
   (directory-abbrev-alist
    (let ((uln (user-login-name)))
      (list (cons (file-name-concat "/var/home/" (user-login-name)) "~")
            (cons (file-name-concat "/home/" (user-login-name)) "~"))))
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
   (indicate-empty-lines t)
   (inhibit-startup-screen t)
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
   (register-use-preview nil)
   (repeat-mode t)
   (require-final-newline t)
   (ring-bell-function #'ignore)
   (scroll-conservatively 101)
   (show-paren-context-when-offscreen t)
   (suggest-key-bindings nil)
   (tab-always-indent 'complete)
   (tooltip-delay 0.1)
   (use-dialog-box nil)
   (use-short-answers t)
   (vc-follow-symlinks nil)
   (view-read-only t)
   (warning-minimum-level :emergency)
   (mouse-wheel-scroll-amount '(1))
   (trusted-content (list (locate-user-emacs-file "lisp/lina/")
                          (locate-user-emacs-file "user-lisp/")))
   (mode-line-modes
    `((compilation-in-progress
       ,(propertize "[Compiling] "
	            'help-echo "Compiling; mouse-1: Goto Buffer"
                    'mouse-face 'mode-line-highlight
                    'local-map (make-mode-line-mouse-map
                                'mouse-1
			        #'compilation-goto-in-progress-buffer)))
      ,(propertize "%[" 'help-echo #1="Recursive edit, type C-M-c to get out")
      "("
      (:propertize (""
                    (:eval
                     (if (listp mode-name)
                         (format-mode-line (cons (symbol-name major-mode)
                                                 (cdr mode-name)))
                       (if (memq major-mode '(c-mode c-ts-mode))
                           mode-name
                         (symbol-name major-mode)))))
                   help-echo "Major mode
mouse-1: Display major mode menu
mouse-2: Show help for major mode
mouse-3: Toggle minor modes"
                   mouse-face mode-line-highlight
                   local-map ,(make-mode-line-mouse-map
                               'mouse-1
                               #'derived-modes?))
      ("" mode-line-process)
      ,(propertize "%n" 'help-echo "mouse-2: Remove narrowing from buffer"
		   'mouse-face 'mode-line-highlight
		   'local-map (make-mode-line-mouse-map
			       'mouse-2 #'mode-line-widen))
      ("" mode-line-minor-modes)
      ")"
      ,(propertize "%]" 'help-echo #1#)
      " "))
   (mode-line-percent-position nil)
   (mode-line-format
    '("%e"
      mode-line-front-space
      mode-line-mule-info
      mode-line-client
      mode-line-modified
      mode-line-remote
      mode-line-window-dedicated
      " "
      mode-line-buffer-identification
      "  "
      mode-line-position
      (project-mode-line project-mode-line-format)
      " "
      mode-line-modes
      mode-line-misc-info))
   (frame-title-format
    `("(%F) "
      ("" (:eval (let* ((current (current-buffer))
                        (buffer (if (string-equal (buffer-name current) "*Help*")
                                    (window-buffer (previous-window))
                                  nil))
                        (bfn (buffer-file-name buffer)))
                   (if bfn
                       (abbreviate-file-name bfn)
                     (buffer-name)))))
      " - Emacs@" ,system-name "<" ,(number-to-string (emacs-pid)) ">"))
   (text-quoting-style 'grave))
  :hook
  (after-init-hook . (lambda ()
                       (remove-hook 'completion-at-point-functions
                                    #'tags-completion-at-point-function)))
  (after-save-hook . font-lock-update)
  :bind
  (("M-u" . ignore)
   ("M-;" . comment-line)
   ("C-l" . redraw-display)
   ("C-x C-g" . ignore)
   ("M-<up>" . backward-up-list)
   ("M-<down>" . down-list)
   ("M-<left>" . backward-sexp)
   ("M-<right>" . forward-sexp)
   ("M-r" . replace-regexp)
   ("C-k" . kill-whole-line)
   ("C-n" . goto-line)
   ("C-t" . transpose-lines)
   ("C-z" . undo)
   ("C-S-z" . undo-redo)
   ("M-z" . undo-redo)
   ("C-," . pop-to-mark-command)
   ("C-x x" . revert-buffer-quick)
   ("C-x C-x" . revert-buffer-quick)
   (:map visual-line-mode-map
         ([remap move-end-of-line] . nil)
         ([remap move-beginning-of-line] . nil))))

;;;; core hooks
(use-package exec-path-from-shell
  :ensure t
  :custom ((exec-path-from-shell-variables '("PATH"
                                             "MANPATH"
                                             "INFOPATH"
                                             "SSH_AUTH_SOCK")))
  :init
  (let ((zsh (executable-find "zsh")))
    (if zsh
        (setopt exec-path-from-shell-shell-name zsh
                exec-path-from-shell-arguments nil)))
  :hook (emacs-startup-hook . exec-path-from-shell-initialize))

(use-package server
  :ensure nil
  :autoload server-running-p
  :hook (emacs-startup-hook . (lambda ()
                                (unless (server-running-p)
                                  (server-start)))))

;;;; theme
(use-package modus-themes
  :functions (modus-themes--with-colors-get-palette
              modus-themes-get-current-theme)
  :ensure t
  :pin gnu
  :custom ((modus-operandi-palette-overrides
            `(
              ;; (border-mode-line-active nil)
              ;; (border-mode-line-inactive nil)
              )
            ))
  :autoload modus-themes-load-theme
  :config
  (defun lina-modus-operandi-hook ()
    (modus-themes-with-colors
      (custom-set-faces
       ;; `(font-lock-warning-face ((,c ( :foreground "#fffff"
       ;;                                 :background ,warning))))
       )))
  :hook ((modus-themes-after-load-theme-hook . lina-modus-operandi-hook)
         (window-setup-hook . (lambda ()
                                (modus-themes-load-theme 'modus-operandi)))))

;;;; fonts
(set-frame-font "Iosevka-10.5" nil t)
(set-face-attribute 'fixed-pitch-serif nil :inherit 'fixed-pitch)

;;;; terminal
(use-package term/xterm
  :ensure nil
  :custom
  ((xterm-mouse-mode t)
   (xterm-set-window-title t)
   (xterm-extra-capabilities '(modifyOtherKeys
                               reportBackground
                               getSelection
                               setSelection))))

;;; completion

(use-package minibuffer
  :ensure nil
  :custom
  (completion-ignore-case t)
  (completion-pcm-leading-wildcard t)
  (minibuffer-nonselected-mode nil)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  :custom-face
  (completions-annotations ((t :underline nil :inherit (italic shadow))))
  :bind
  ("M-i" . completion-at-point)
  (:map minibuffer-local-map
        ("C-u" . kill-whole-line)))

;;;; movec

(use-package marginalia
  :ensure t
  :if (>= emacs-major-version 31)
  :custom (marginalia-mode t))

(use-package orderless
  :demand t
  :ensure t
  :pin gnu
  :custom
  (completion-styles '(emacs22 partial-completion orderless))
  (completion-category-overrides
   `((multi-category (styles substring))
     (buffer (styles substring))
     ,@(mapcar (lambda (cat)
                 (list cat '(styles orderless)))
               '(command symbol function variable symbol-help))))
  (orderless-component-separator "[- ]"))

(use-package vertico
  :ensure t
  :custom
  (vertico-mode t)
  (vertico-count-format nil)
  (vertico-group-format "%s")
  (vertico-multiform-mode t)
  (vertico-multiform-categories '((file (:keymap . vertico-directory-map)))))

(use-package embark
  :defines embark-general-map embark-target-finders
  :ensure t
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
  (defun embark-isearch-symbol-backward ()
    "`embark-isearch-backward' but in symbol mode."
    (interactive)
    (isearch-mode nil nil nil nil 'isearch-symbol-regexp)
    (isearch-edit-string))
  :bind
  (("C-." . embark-act)
   ("M-." . embark-dwim)
   ("C-x ." . embark-act)
   (:map help-map
         ("b" . embark-bindings))
   (:map embark-general-map
         ("C-s" . embark-isearch-symbol-forward)
         ("C-r" . embark-isearch-symbol-backward))
   (:map embark-symbol-map
         ("RET" . embark-find-definition))
   (:map minibuffer-local-map
         ("C-<return>" . embark-export))))

(use-package consult
  :ensure t
  :commands (consult-ripgrep consult-grep)
  :custom ((consult-async-split-style nil)
           (consult-preview-key nil)
           (completion-in-region-function #'consult-completion-in-region)
           (xref-show-xrefs-function #'consult-xref))
  :init
  (unless (package-installed-p 'embark-consult)
    (package-install 'embark-consult))
  (with-eval-after-load 'info
    (defvar Info-mode-map)
    (bind-key "s" #'consult-info Info-mode-map))
  (unless (fboundp 'consult-compile-error)
    (autoload 'consult-compile-error "consult-compile" nil t))
  :bind
  ("M-g" . consult-imenu)
  ("C-c b" . consult-bookmark)
  ("C-c `" . consult-compile-error)
  ("M-s o" . consult-line)
  (:map ctl-x-map
        ("b" . consult-buffer)
        ("r" . consult-register-store)
        ("j" . consult-register-load))
  (:map project-prefix-map
        ("f" . consult-find)
        ("g" . consult-grep)
        ("r" . consult-ripgrep))
  (:map help-map
        ("i" . consult-info)))

;;;; buffer completion

(use-package cape
  :ensure t
  :pin gnu
  :custom (cape-elisp-symbol-wrapper nil)
  :autoload (cape-capf-interactive
             cape-capf-super)
  :commands (cape-elisp-symbol)
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev 0)
  (add-hook 'completion-at-point-functions #'cape-file -10)
  :bind
  ("M-/" . cape-dabbrev)
  ("M-f" . cape-file))

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
  (global-corfu-minibuffer t)
  (global-corfu-mode t)
  (global-corfu-modes '((not comint-mode eshell-mode) t))
  :bind (:map corfu-map
              ("C-g" . corfu-quit)
              ("TAB" . corfu-next)
              ("<backtab>" . corfu-previous)))

;;; help

(use-package cus-edit
  :ensure nil
  :custom ((custom-unlispify-tag-names nil))
  :bind
  (:map help-map
        ("g" . customize-group-other-window)
        ("u" . customize-variable-other-window))
  (:map custom-field-keymap
        ("<down-mouse-1>" . nil)))

(use-package help
  :ensure nil
  :custom
  (help-window-select t)
  :bind
  (("C-h m" . describe-keymap)
   ("C-h F" . describe-face)
   ("C-h C-g" . help-quit)
   ("C-h C-h" . nil)
   (:map help-mode-map
         ("," . help-go-back)
         ("p" . help-go-back))))

(use-package info
  :ensure nil
  :defines Info-mode-map
  :bind
  (:map Info-mode-map
        ("R" . info-display-manual))
  (:map help-map
        ("s" . info-lookup-symbol)))

(use-package man
  :ensure nil
  :custom (Man-notify-method 'thrifty))

;;; built-in minor modes

;;; built-in global minor modes

(use-package recentf
  :ensure nil
  :custom
  (recentf-mode t)
  (recentf-max-saved-items nil)
  (recentf-exclude `(,(rx bos "/nix/store/" (* nonl)))))

(use-package eldoc
  :ensure nil
  :custom
  ((eldoc-minor-mode-string nil)
   (eldoc-echo-area-use-multiline-p nil)
   (eldoc-documentation-strategy #'eldoc-documentation-compose)))

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

(use-package autorevert
  :ensure nil
  :custom
  ((global-auto-revert-mode t)
   (auto-revert-mode-text "")))

(use-package winner
  :ensure nil
  :custom (winner-mode t))

;;;; built-in local minor modes

(use-package flymake
  :ensure nil
  :functions flymake-eldoc-function
  :config
  (defun lina-flymake-hook ()
    (when flymake-mode
      (setq-local eldoc-documentation-functions
                  (cons #'flymake-eldoc-function
                        (delq #'flymake-eldoc-function
                              eldoc-documentation-functions)))))
  :hook ((flymake-mode-hook . lina-flymake-hook)
         ((sh-base-mode-hook python-base-mode-hook) . flymake-mode))
  :bind
  (:map flymake-mode-map
        ("C-x m" . flymake-show-buffer-diagnostics)))

(use-package display-line-numbers
  :ensure nil
  :custom
  ((display-line-numbers-grow-only t)
   (display-line-numbers-width 4))
  :hook ((prog-mode-hook markdown-ts-mode-hook) . display-line-numbers-mode))

(use-package goto-addr
  :ensure nil
  :hook ((prog-mode-hook . goto-address-prog-mode)
         (text-mode-hook . goto-address-mode)))

;;; built-in commands

(use-package project
  :ensure nil
  :custom
  ((project-switch-commands #'project-dired))
  :config
  (fset 'project-prefix-map project-prefix-map)
  (defun lina-project-save-buffers ()
    (interactive)
    (project-save-some-buffers t))
  :bind (("C-p" . project-prefix-map)
         (:map project-prefix-map
               ("d" . project-dired)
               ("s" . project-eshell)
               ("C-f" . project-or-external-find-file)
               ("C-s" . lina-project-save-buffers))
         (:map mode-specific-map
               ("C-c" . project-recompile))))

(use-package xref
  :ensure nil
  :custom
  (xref-prompt-for-identifier nil)
  (xref-show-definitions-function #'xref-show-definitions-completing-read)
  (xref-search-program 'ripgrep))

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
  :bind
  ("C-x F" . find-function-other-window)
  ("C-x L" . find-library-other-window)
  ("C-h K" . find-function-on-key))

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

(use-package tetris
  :ensure nil
  :bind (:map tetris-mode-map
              ("z" . tetris-rotate-next)
              ("x" . tetris-rotate-prev)))

(use-package hi-lock
  :ensure nil
  :bind ("M-s h" . highlight-symbol-at-point))

(use-package re-builder
  :ensure nil
  :custom ((reb-re-syntax 'read)))

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
  ((compile-command (format "make -k -j%d " (num-processors)))
   (compilation-scroll-output 'first-error)
   (compilation-ask-about-save nil)
   (compilation-process-setup-function
    #'hack-dir-local-variables-non-file-buffer)))

(use-package auth-source
  :ensure nil
  :custom (auth-sources '("~/.authinfo")))

;;;; shells

(use-package shell
  :ensure nil
  :defines shell-mode
  :custom
  ((shell-kill-buffer-on-exit t)
   (explicit-shell-file-name (or
                              (let ((zsh (executable-find "zsh")))
                                (and (file-exists-p "~/.zshrc")
                                     zsh))
                              "/bin/bash")))
  :config
  (defun lina/shell-hook ()
    (setq-local comint-process-echoes t))
  :hook (shell-mode-hook . lina/shell-hook))

(use-package term
  :ensure nil
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
                               ("sudo" "dnf" "install"))))

;;; third-party integrations

(use-package dumb-jump
  :ensure t
  :custom
  ((dumb-jump-prefer-searcher 'rg))
  :init
  (setq-default xref-backend-functions '(dumb-jump-xref-activate)))

(use-package magit
  :functions (magit-clone-read-args
              magit-branch-read-args
              lina/vertico-preselect-around)
  :preface
  (setq-default magit-define-global-key-bindings nil)
  :custom
  (magit-display-buffer-function #'display-buffer)
  (magit-commit-show-diff nil)
  :config
  (defun lina/vertico-preselect-around (func &rest args)
    (let ((vertico-preselect 'prompt))
      (apply func args)))
  (advice-add #'magit-clone-read-args :around #'lina/vertico-preselect-around)
  (advice-add #'magit-branch-read-args :around #'lina/vertico-preselect-around)
  :bind
  ("C-x g" . magit-dispatch)
  (:map mode-specific-map
        ("g" . magit-file-dispatch)))

(use-package git-commit
  :functions git-commit-collapse-diff
  :config
  (setq git-commit-setup-hook (delq #'git-commit-collapse-diff
                                    git-commit-setup-hook))
  :hook (git-commit-mode-hook . display-fill-column-indicator-mode))

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
  :hook (eshell-mode-hook . with-editor-export-editor))

(use-package delight
  :ensure t)

(use-package ghostel
  :functions ghostel-module-compile@no-colour
  :defines ghostel-semi-char-mode-map
  :custom
  ((ghostel-shell (or (executable-find "zsh") "/bin/bash"))
   (ghostel-term "xterm-256color")
   (ghostel-module-auto-install nil)
   (ghostel-module-compile-command "zig build --color off --prefix %s -Doptimize=ReleaseFast -Dcpu=baseline")
   (ghostel-keymap-exceptions '("C-c" "C-x" "C-h" "M-x" "M-:" "C-g"))
   (ghostel-point-leave-input-mode nil))
  :config
  (defun lina-ghostel-pre-spawn-hook ()
    (when (fboundp 'with-editor--setup)
      (let ((with-editor--envvar "EDITOR"))
        (with-editor--setup))))
  :hook (ghostel-pre-spawn-hook . lina-ghostel-pre-spawn-hook)
  :bind
  (:map ghostel-semi-char-mode-map
        ("C-c C-u" . universal-argument))
  (:map project-prefix-map
        ("t" . ghostel-project)))

(use-package apheleia
  :defines apheleia-mode-alist python-mode
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
     (tex-mode . tex-fmt)))
  :config
  (setq-mode-local python-mode apheleia-formatters-respect-fill-column t)
  :hook ((tex-mode-hook python-base-mode-hook) . apheleia-mode))

;;; third-party minor modes

(use-package gcmh
  :ensure t
  :delight gcmh-mode
  :hook (after-init-hook . gcmh-mode))

(use-package sly
  :custom ((inferior-lisp-program "sbcl")
           (sly-complete-symbol-function #'sly-simple-completions)
           (sly-symbol-completion-mode nil))
  :config
  (setf (alist-get common-lisp-hyperspec-root browse-url-handlers nil nil #'string-equal)
        #'eww-browse-url)
  :bind (:map sly-mode-map
              ("C-c f" . sly-describe-function)
              ("C-c v" . sly-describe-symbol)
              ("C-c i" . sly-documentation-lookup)
              ("C-c C-p" . sly-macroexpand-1)))

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

(use-package ibuffer
  :ensure nil
  :custom ((ibuffer-default-sorting-mode 'filename/process)))

;;; built-in language major modes

(use-package prog-mode
  :ensure nil
  :init
  (defun lina/c-w-dwim ()
    (interactive)
    (call-interactively (if (use-region-p) #'kill-region #'backward-kill-sexp)))
  :config
  (defun lina/prog-mode-hook ()
    (if (fboundp 'delete-trailing-whitespace-mode)
        (delete-trailing-whitespace-mode t)
      (add-hook 'before-save-hook #'delete-trailing-whitespace nil t)))
  :hook (prog-mode-hook . lina/prog-mode-hook)
  :bind (:map prog-mode-map
              ("DEL" . backward-delete-char-untabify)
              ("C-w" . lina/c-w-dwim)))

(use-package text-mode
  :ensure nil
  :hook (text-mode-hook . visual-line-mode))

(use-package treesit
  :ensure nil
  :defines treesit-language-source-alist
  :custom
  (treesit-auto-install-grammar 'always)
  (treesit-enabled-modes '(bash-ts-mode
                           json-ts-mode
                           typescript-ts-mode
                           tsx-ts-mode
                           python-ts-mode
                           markdown-ts-mode))
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
  :config
  (defun lina-js-hook ()
    (setq-local eldoc-echo-area-use-multiline-p t))
  :hook ((js-base-mode-hook typescript-ts-base-mode-hook) . lina-js-hook)
  :mode ((rx ".conflist" eos) . js-json-mode))

(use-package typescript-ts-mode
  :ensure nil
  :config
  (defun lina-jsx-hook ()
    nil)
  :hook (tsx-ts-mode-hook . lina-jsx-hook))

(use-package sh-script
  :ensure nil
  :custom
  (sh-basic-offset 2)
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
  ((python-flymake-command '("uvx"
                             "ruff"
                             "check"
                             "--quiet"
                             "--output-format=concise"
                             "--stdin-filename=stdin"))
   (python-indent-guess-indent-offset-verbose nil)
   (python-shell-dedicated 'buffer))
  :config
  (defun lina/python-mode-hook ()
    (setq-local fill-column 79
                tab-always-indent t))
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
-synctex=1"))
  :hook (tex-mode-hook . lina/tex-hook)
  :bind (:map latex-mode-map
              ("C-c C-c" . recompile)))

(use-package backtrace
  :ensure nil
  :config
  (defun lina/backtrace-mode-hook ()
    (setq-local truncate-lines nil))
  :hook (backtrace-mode-hook . lina/backtrace-mode-hook))

(use-package image-mode
  :ensure nil
  :bind
  (:map image-mode-map
        ([remap revert-buffer] . revert-buffer-quick)))

(use-package wid-edit
  :bind
  (:map widget-keymap
        ("SPC" . widget-button-press)
        ("M-i" . widget-complete))
  (:map widget-field-keymap
        ("M-i" . widget-complete))
  (:map widget-text-keymap
        ("M-i" . widget-complete)))

(use-package markdown-ts-mode
  :ensure nil
  :mode "\\.md\\'")

(use-package make-mode
  :ensure nil
  :config
  (defun lina-makefile-hook ()
    (setq-local whitespace-style '(face tabs tab-mark))
    (whitespace-mode t))
  (unbind-key "C-c C-c" makefile-mode-map)
  :hook (makefile-mode-hook . lina-makefile-hook))

;;; third-party major modes

(use-package nix-ts-mode
  :init
  (setf (alist-get 'nix treesit-language-source-alist)
        '("https://github.com/nix-community/tree-sitter-nix.git"
          "v0.3.0")
        (alist-get 'nix-mode major-mode-remap-alist) 'nix-ts-mode)
  :mode "\\.nix\\'")

(autoload 'inheritenv-add-advice "inheritenv" "Advise function FUNC with ‘inheritenv-apply’.
This will ensure that any buffers (including temporary buffers)
created by FUNC will inherit the caller’s environment.

(fn FUNC)" nil 'macro)

(use-package kubed
  :vc (:url "https://git.sr.ht/~eshel/kubed" :rev "master")
  :bind
  ("C-c k" . kubed-transient))

(use-package yaml-mode
  :ensure t
  :mode ((rx "." (or "yaml" "yml") eos)))

;;; other files

(load "lina-window")
(load "lina-puni")
(load "lina-elisp")
(load "lina-mail")
(load "lina-eglot")
(load "lina-org")
(load "lina-c")
(load "lina-llm")

(use-package eval-expression-and-save
  :ensure nil
  :load-path (lambda () user-lisp-directory)
  :bind
  ("M-:" . eval-expression-and-save))

(use-package check-json
  :ensure nil
  :load-path (lambda () user-lisp-directory)
  :hook (json-ts-mode-hook . check-json-enable-in-buffer))

(use-package initialise-file-path
  :demand t
  :ensure nil
  :load-path (lambda () user-lisp-directory)
  :config
  (setopt mode-line-buffer-identification
          (let ((lmb (lambda ()
                       (interactive)
                       (message "%s" (buffer-file-name))))
                (rmb (lambda ()
                       (interactive)
                       (let ((bfn (buffer-file-name)))
                         (if (not bfn)
                             (message "Buffer has no file name")
                           (kill-new bfn)
                           (message "Copied %s" bfn))))))
            `(:propertize
              (:eval (if-let* ((bfn (buffer-file-name)))
                         (initialise-file-path bfn)
                       "%12b"))
              face mode-line-buffer-id
              mouse-face mode-line-highlight
              local-map
              ,(define-keymap
                 "<mode-line> <mouse-1>"        lmb
                 "<mode-line> <mouse-3>"        rmb
                 "<header-line> <mouse-1>"      #'mode-line-previous-buffer
                 "<header-line> <mouse-3>"      #'mode-line-next-buffer
                 "<header-line> <down-mouse-3>" #'ignore)))))

;;; init.el ends here
