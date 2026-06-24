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
(with-eval-after-load 'package-vc
  (setq-default vc-handled-backends '(Git)))

;;;; emacs

(use-package emacs
  :ensure nil
  :preface
  (defconst linux-font '(:family "Iosevka" :height 105))
  :init
  (setopt auth-sources '("~/.authinfo")
          backward-delete-char-untabify-method 'hungry
          bidi-inhibit-bpa t
          bidi-paragraph-direction 'left-to-right
          create-lockfiles nil
          cursor-in-non-selected-windows nil
          enable-recursive-minibuffers t
          fast-but-imprecise-scrolling t
          fill-column 80
          garbage-collection-messages t
          mouse-autoselect-window t
          native-comp-async-on-battery-power nil
          native-comp-async-report-warnings-errors 'silent
          read-process-output-max 1048576
          redisplay-skip-fontification-on-input t
          max-redisplay-ticks 1000000
          ring-bell-function #'ignore
          scroll-conservatively 100
          tab-always-indent 'complete
          tooltip-delay 0.1
          use-dialog-box nil
          use-short-answers t
          vc-follow-symlinks t
          warning-minimum-level :emergency
          xterm-set-window-title t
          xterm-mouse-mode t)
  :custom-face
  (default ((((type x pgtk)) ,linux-font)))
  (fixed-pitch ((((type x pgtk)) ,linux-font)))
  (fixed-pitch-serif ((t :inherit (fixed-pitch))))
  :bind (("M-u" . ignore)
         ("M-;" . comment-line)
         ("C-l" . redraw-display)
         ;; ("C-x m" . push-point-to-register)
         ("C-x j" . register-to-point)
         ("C-x C-g" . ignore)))

(use-package startup
  :ensure nil
  :custom
  ((inhibit-startup-screen t)
   (initial-scratch-message nil)
   (initial-major-mode 'fundamental-mode)))

(use-package simple
  :ensure nil
  :custom
  ((column-number-mode t)
   (extended-command-suggest-shorter nil)
   (indent-tabs-mode nil)
   (kill-do-not-save-duplicates t)
   (kill-region-dwim 'emacs-word)
   (read-extended-command-predicate #'command-completion-default-include-p)
   (suggest-key-bindings nil))
  :config
  (when (fboundp 'delete-trailing-whitespace-mode)
    (add-hook 'prog-mode-hook #'delete-trailing-whitespace-mode))
  :bind (("C-k" . kill-whole-line)
         ("C-z" . undo)
         ("C-S-z" . undo-redo)
         ("M-," . pop-to-mark-command)
         ("C-," . pop-global-mark)))

(use-package files
  :ensure nil
  :custom
  ((auto-save-default nil)
   (confirm-kill-processes nil)
   (make-backup-files nil)
   (require-final-newline t)
   (view-read-only t))
  :bind
  ("C-x x" . revert-buffer-quick)
  ("C-x C-x" . revert-buffer-quick))

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
   `(
     ((derived-mode . Info-mode)
      display-buffer-reuse-mode-window)
     ((derived-mode . magit-diff-mode)
      (display-buffer-reuse-mode-window)
      (mode . magit-log-mode))
     ("\\*Completions"
      (display-buffer-reuse-window
       display-buffer-at-bottom))
     ((derived-mode . calc-mode)
      display-buffer-at-bottom)
     ((and
       (not ,(rx bos "*Async Shell Command*" eos))
       (or
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
              "Warnings"
              "Compile-Log"))))
      display-buffer-in-side-window
      (window-height . 12)
      (slot . 0))
     ((or
       (category . man)
       (major-mode . Man-mode)
       ,(rx bos "*Man"))
      (display-buffer-reuse-mode-window)
      (mode . Man-mode))
     (,(rx bos "*Customize")
      display-buffer-reuse-mode-window)
     (,(rx bos "*Pp")
      (display-buffer-reuse-window
       display-buffer-below-selected)
      (dedicated . t))))
  (switch-to-buffer-in-dedicated-window 'pop)
  (switch-to-buffer-obey-display-actions t)
  (split-window-preferred-direction 'horizontal)
  :config
  (defun split-window-right-and-select (&rest args)
    (interactive)
    (select-window (apply #'split-window-right args)))
  (defun split-window-below-and-select (&rest args)
    (interactive)
    (select-window (apply #'split-window-below args)))
  :bind
  ("C-x 2" . split-window-below-and-select)
  ("C-x 3" . split-window-right-and-select)
  ("C-x 5" . make-frame-command)
  ("C-x q" . quit-window)
  ("C-x o" . other-window)
  ("C-x 4" . other-window-prefix))

(require-theme 'modus-themes)
(add-hook 'after-init-hook (apply-partially #'load-theme 'modus-operandi t))

(use-package display-fill-column-indicator
  :ensure nil
  :custom
  (display-fill-column-indicator-character nil)
  (global-display-fill-column-indicator-mode t)
  (global-display-fill-column-indicator-modes '(prog-mode))
  :custom-face
  (fill-column-indicator ((t :foreground ,"grey" :background unspecified))))

;;; minibuffer

(use-package minibuffer
  :ensure nil
  :custom-face
  (completions-annotations ((t :underline nil :inherit (italic shadow))))
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

(use-package saveplace
  :ensure nil
  :custom
  (save-place-mode t))

;;; integrations

(use-package tramp
  :ensure nil
  :demand t
  :autoload tramp-recentf-cleanup tramp-recentf-cleanup-all tramp-enable-method
  :custom
  (tramp-show-ad-hoc-proxies t)
  :config
  (advice-add #'tramp-recentf-cleanup :override #'ignore)
  (advice-add #'tramp-recentf-cleanup-all :override #'ignore)
  (tramp-enable-method 'podman)
  (tramp-enable-method 'distrobox))

(use-package shell
  :ensure nil
  :custom
  (shell-kill-buffer-on-exit t))

(use-package esh-mode
  :ensure nil
  :autoload eshell-reset
  :config
  (defun lina/eshell-hook ()
    (electric-pair-local-mode -1))
  :hook (eshell-mode-hook . lina/eshell-hook)
  :bind
  (:map eshell-mode-map
        ("C-w" . unix-word-rubout)
        ("C-u" . eshell-kill-input)
        ("C-d" . eshell-send-eof-to-process)))

(use-package eshell
  :ensure nil
  :functions eshell/cd
  :custom
  (eshell-scroll-to-bottom-on-input t)
  (eshell-visual-subcommands '(("bootc" "update")))
  :config
  (defun lina/eshell-in-buffer-directory ()
    (interactive)
    (let ((bufdir default-directory))
      (with-current-buffer (eshell)
        (unless (string= bufdir default-directory)
          (eshell/cd `(,bufdir))
          (eshell-reset)))))
  :bind
  (:map project-prefix-map
        ("s" . lina/eshell-in-buffer-directory)))

(use-package flymake
  :defines emacs-lisp-mode
  :ensure nil
  :hook
  ((sh-base-mode-hook python-mode-hook) . flymake-mode)
  :config
  (unless (display-graphic-p)
    (setq-default flymake-show-diagnostics-at-end-of-line 'short))
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
            (eglot-inlay-hints-mode (if (member major-mode
                                                '(python-mode python-ts-mode))
                                        -1
                                      t))
            (add-hook 'flymake-diagnostic-functions
                      #'eglot-flymake-backend nil t)
            (flymake-mode t)))

(use-package ispell
  :ensure nil
  :custom
  (ispell-dictionary "en_GB"))

(use-package browse-url
  :ensure nil
  :custom
  (browse-url-handlers `((,(rx ".pdf" eos) . browse-url--browser))))

;;; built-in major modes

(use-package prog-mode
  :ensure nil
  :bind (:map prog-mode-map
              ("M-<down>" . down-list)))

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
                           bash-ts-mode))
  (treesit-font-lock-level 4))

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
        ("R" . info-display-manual))
  (:map help-map
        ("s" . info-lookup-symbol)))

(use-package man
  :ensure nil
  :functions Man-notify-when-ready@display-buffer
  :config
  (define-advice Man-notify-when-ready (:override (buffer) display-buffer)
    "Call `display-buffer' with BUFFER and action (category . man)."
    (display-buffer buffer '(nil (category . man)))))

(use-package conf-mode
  :ensure nil
  :mode
  (((rx "." (or "container" "volume" "service" "pod") eos) . conf-desktop-mode)
   ((rx "/isyncrc" eos) . conf-space-mode)
   ((rx ".ovpn" eos) . conf-space-mode)
   ((rx "/" (or "sysusers.d" "tmpfiles.d") "/" (+ any) ".conf" eos)
    .
    conf-space-mode)))

(use-package js
  :ensure nil
  :custom
  (js-indent-level 2)
  :mode ((rx ".conflist" eos) . js-json-mode))

(use-package sh-script
  :ensure nil
  :custom
  (sh-basic-offset 2)
  :mode
  ((rx "/.env" (opt ".local")) . sh-mode))

(use-package elisp-mode
  :ensure nil
  :defines emacs-lisp-mode
  :config
  (defun lina/elisp-hook ()
    (catch 'lina/elisp-hook
      (when (and (buffer-file-name)
                 (file-in-directory-p (buffer-file-name) package-user-dir))
        (view-mode)
        (throw 'lina/elisp-hook nil)))
    (setq-local
     outline-regexp (rx (and ";;;" (0+ ";") " " (not (any blank))))
     outline-imenu-generic-expression `(("Headings" ,(rx bol (regexp outline-regexp) (0+ any)) 0))
     ;; "^\\(?:" outline-regexp "\\).*$"
     imenu-generic-expression (append outline-imenu-generic-expression
                                      imenu-generic-expression)
     flymake-diagnostic-functions '(elisp-flymake-byte-compile t))
    (flymake-mode t)
    (when (fboundp 'corfu-mode)
      (corfu-mode t)))
  (defun autoload-cookie ()
    "Insert an autoload cookie above the current defun."
    (interactive)
    (save-excursion
      (beginning-of-defun)
      (insert ";;;###autoload\n")))
  :hook (emacs-lisp-mode-hook . lina/elisp-hook)
  :bind (:map emacs-lisp-mode-map
              ("C-c C-c" . elisp-eval-region-or-buffer)))

(use-package comint
  :ensure nil
  :autoload comint-skip-input
  :custom
  (comint-prompt-read-only t)
  :bind (:map comint-mode-map
              ("<up>" . comint-previous-input)
              ("<down>" . comint-next-input)))

(use-package ielm
  :ensure nil
  :commands ielm-return
  :config
  (defun lina/ielm-interrupt ()
    (interactive)
    (comint-skip-input)
    (ielm-return))
  :bind (:map inferior-emacs-lisp-mode-map ("C-c C-c" . lina/ielm-interrupt)))

(use-package dockerfile-ts-mode
  :ensure nil
  :defines dockerfile-ts-mode
  :config
  (setq-mode-local dockerfile-ts-mode indent-line-function
                   #'indent-relative-first-indent-point)
  :mode ((rx (or "Docker" "Container") "file" (* any) eos)))

(use-package python
  :ensure nil
  :custom
  (python-flymake-command '("ruff"
                            "check"
                            "--quiet"
                            "--output-format=concise"
                            "--stdin-filename=stdin"))
  (python-indent-guess-indent-offset-verbose nil))

(use-package image-mode
  :ensure nil
  :bind (:map image-mode-map
              ([remap revert-buffer] . revert-buffer-quick)))

;;; built-in minor modes

(use-package pp
  :ensure nil
  :functions pp-display-expression@readonly
  :config
  (define-advice pp-display-expression
      (:after (_expression out-buffer-name &optional _lisp) readonly)
    (with-current-buffer out-buffer-name
      (view-mode))
    (pop-to-buffer out-buffer-name))
  :bind
  ("M-:" . pp-eval-expression)
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

(use-package delsel
  :ensure nil
  :custom
  (delete-selection-mode t))

(use-package repeat
  :ensure nil
  :custom
  (repeat-mode t))

(use-package outline
  :ensure nil
  :custom
  (outline-minor-mode-prefix (kbd "C-c ;")))

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
  (consult-ripgrep-args "rg --null --line-buffered --color=never --max-columns=1000 --path-separator /   --smart-case --no-heading --with-filename --line-number --search-zip --glob=!TAGS")
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
        ("C-r" . consult-recent-file)
        ("r" . consult-bookmark))
  (:map project-prefix-map
        ("g" . consult-ripgrep-or-grep)
        ("f" . consult-find))
  (:map help-map
        ("i" . consult-info))
  (:package info :map Info-mode-map
            ("s" . consult-info)))

(use-package embark
  :defines embark-general-map
  :ensure t
  :pin gnu
  :custom
  (embark-cycle-key "TAB")
  (embark-indicators '(embark-minimal-indicator
                       embark-highlight-indicator
                       embark-isearch-highlight-indicator))
  (prefix-help-command #'embark-prefix-help-command)
  :config
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
  (completion-category-overrides `((buffer (styles substring))
                                   ,@(mapcar (lambda (cat)
                                               (list cat '(styles orderless)))
                                             '(command symbol function variable symbol-help))))
  (orderless-component-separator "[- ]")
  :config
  (setq-mode-local emacs-lisp-mode completion-styles '(orderless)))

(use-package corfu
  :defines corfu-map
  :if (or (display-graphic-p)
          (>= emacs-major-version 31))
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

(use-package marginalia
  :ensure t
  :disabled t
  :custom
  (marginalia-mode t))

;;; third-party minor modes

(require 'lina-puni)

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

(use-package magit
  :pin nongnu
  :preface
  (setq magit-define-global-key-bindings nil)
  :custom
  (magit-display-buffer-function #'display-buffer)
  (magit-commit-show-diff nil)
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

(use-package ruff-format
  :disabled t
  :hook (python-mode-hook . ruff-format-on-save-mode))

(use-package delight
  :ensure t)

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

(require 'lina-mail)
