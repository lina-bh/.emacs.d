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
                                 ("gnu-devel" . "https://elpa.gnu.org/devel/")
                                 ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                                 ("nongnu-devel"
                                  .
                                  "https://elpa.nongnu.org/nongnu-devel/")
                                 ("melpa"
                                  . "https://snapshots.melpa.org/packages/")
                                 ("melpa-stable"
                                  . "https://releases.melpa.org/packages/"))
              package-archive-priorities '(("gnu" . 3)
                                           ("nongnu" . 2)
                                           ("melpa-stable" . 1))
              package-pinned-packages '((smartparens . "melpa-stable")
                                        (ghostel . "melpa")
                                        (package-lint . "nongnu-devel")
                                        (terraform-mode . "melpa")
                                        (hcl-mode . "melpa")))
(package-initialize)

(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file t)

;;; some functions

(defun lina-backward-delete-word (arg)
  (interactive "p")
  (if (use-region-p)
      (call-interactively #'kill-region)
    (delete-region (point) (progn
                             (forward-word (- arg))
                             (point)))))

;;; emacs
(use-package emacs
  :ensure nil
  :demand t
  :no-require t
  :custom
  ((auto-insert-query nil)
   (async-shell-command-buffer 'new-buffer)
   (auto-save-default nil)
   (backward-delete-char-untabify-method 'hungry)
   (bidi-inhibit-bpa t)
   (bidi-paragraph-direction 'left-to-right)
   (column-number-mode t)
   (confirm-kill-processes nil)
   (create-lockfiles nil)
   (cursor-in-non-selected-windows nil)
   (delete-selection-mode t)
   (directory-abbrev-alist
    (let ((uln (user-login-name)))
      (list (cons (file-name-concat "/var/home/" (user-login-name)) "~")
            (cons (file-name-concat "/home/" (user-login-name)) "~"))))
   (enable-recursive-minibuffers t)
   (extended-command-suggest-shorter nil)
   (fast-but-imprecise-scrolling t)
   (fill-column 80)
   (garbage-collection-messages t)
   (indent-tabs-mode nil)
   (indicate-empty-lines t)
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
   (pixel-scroll-precision-mode t)
   (prettify-special-glyphs-mode t)
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
   (tooltip-mode nil)
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
                     (let* ((name (symbol-name major-mode))
                            (title (concat (upcase (substring name 0 1))
                                           (substring name 1))))
                       (cond
                        ((memq major-mode '(c-mode c-ts-mode))
                         mode-name)
                        ((listp mode-name)
                         (format-mode-line (cons title
                                                 (cdr mode-name))))
                        (t title)))))
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
    `(("" (:eval (let* ((current (current-buffer))
                        (buffer (if (string-equal (buffer-name current) "*Help*")
                                    (window-buffer (previous-window))
                                  nil))
                        (bfn (buffer-file-name buffer)))
                   (if bfn
                       (abbreviate-file-name bfn)
                     (buffer-name)))))
      "@" ,system-name))
   (text-quoting-style 'grave))
  :config
  (put 'erase-buffer 'disabled nil)
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
   ("C-w" . lina-backward-delete-word)
   ("C-x x" . revert-buffer-quick)
   ("C-x C-x" . revert-buffer-quick)
   ("C-x DEL" . erase-buffer)
   (:map visual-line-mode-map
         ([remap move-end-of-line] . nil)
         ([remap move-beginning-of-line] . nil))))

;;;; core hooks
(use-package exec-path-from-shell
  :ensure t
  :if (not (eq system-type 'windows-nt))
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
  :autoload (server-running-p server-done)
  :config
  (keymap-global-set "C-x #" (defun lina-server-done ()
                               (interactive)
                               (server-done)))
  :hook (emacs-startup-hook . (lambda ()
                                (unless (server-running-p)
                                  (server-start)))))

;;;; theme
(use-package modus-themes
  :ensure t
  :pin gnu
  :custom (modus-themes-mixed-fonts t))

(use-package standard-themes
  :ensure t
  :pin gnu
  :autoload standard-themes-load-theme
  :hook (window-setup-hook . (lambda ()
                               (standard-themes-load-theme 'standard-dark))))

;;;; fonts

(use-package fontaine
  :functions fontaine-set-preset
  :ensure t
  :pin gnu
  :custom
  ((fontaine-presets '((x :default-family "Iosevka Fixed"
                          :default-height 99
                          :fixed-pitch-family "Iosevka Fixed"
                          :fixed-pitch-serif-family "Iosevka Fixed"
                          :variable-pitch-family "Liberation Sans"
                          :variable-pitch-height 1.1)))
   (fontaine-mode t))
  :init
  (cond
   ((featurep 'x)
    (fontaine-set-preset 'x))))

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
  :demand t
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
  :demand t
  :no-require t
  :custom (marginalia-mode t))

(use-package orderless
  :pin gnu
  :ensure t
  :demand t
  :config
  (setopt completion-styles '(partial-completion orderless)
          completion-category-overrides
          `((multi-category (styles substring))
            (buffer (styles substring))
            ,@(mapcar (lambda (cat)
                        (list cat '(styles orderless)))
                      '(command symbol function variable symbol-help)))
          orderless-component-separator "[- ]"))

(use-package vertico
  :pin gnu
  :ensure t
  :no-require t
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
         ("C-RET" . embark-export))))

(use-package consult
  :ensure t
  :commands (consult-ripgrep consult-grep)
  :custom ((consult-async-split-style nil)
           (consult-preview-max-count 0)
           (consult-preview-allowed-hooks '(global-font-lock-mode
                                            save-place-find-file-hook
                                            display-line-numbers-mode))
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
  ("M-o" . consult-line)
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

(use-package abbrev
  :ensure nil
  :custom ((save-abbrevs nil)))

(use-package dabbrev
  :ensure nil
  :custom (dabbrev-case-replace nil))

(use-package skeleton
  :ensure nil
  :init
  (setq-default skeleton-further-elements '((abbrev-mode nil)
                                            (electric-indent-mode nil))))

(use-package cape
  :pin gnu
  :ensure t
  :demand t
  :custom (cape-elisp-symbol-wrapper nil)
  :functions (cape-abbrev
              cape-dabbrev
              cape-file)
  :config
  (add-hook 'completion-at-point-functions #'cape-dabbrev 0)
  (add-hook 'completion-at-point-functions #'cape-abbrev -5)
  (add-hook 'completion-at-point-functions #'cape-file -10)
  :bind
  ("M-/" . cape-dabbrev)
  ("M-f" . cape-file))

(use-package corfu
  :defines corfu-map
  :commands corfu-insert corfu-next
  :ensure t
  :demand t
  :if (or (display-graphic-p)
          (>= emacs-major-version 31))
  :custom
  (corfu-cycle t)
  (corfu-quit-at-boundary t)
  (corfu-quit-no-match nil)
  (corfu-preselect 'first)
  (global-corfu-minibuffer t)
  (global-corfu-mode t)
  (global-corfu-modes t)
  :config
  (defun lina-corfu-tab ()
    (interactive)
    (call-interactively
     (if (derived-mode-p 'comint-mode 'eshell-mode)
         #'corfu-insert
       #'corfu-next)))
  :bind (:map corfu-map
              ("C-g" . corfu-quit)
              ("TAB" . lina-corfu-tab)
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
  :bind (("C-h m" . describe-keymap)
         ("C-h F" . describe-face)
         ("C-h C-g" . help-quit)
         ("C-h C-h" . nil)))

(use-package info
  :ensure nil
  :defines Info-mode-map
  :bind
  (:map Info-mode-map
        ("R" . info-display-manual))
  (:map help-map
        ("r" . info-display-manual)
        ("s" . info-lookup-symbol)))

(use-package man
  :ensure nil
  :custom (Man-notify-method 'thrifty)
  :functions Man-notify-when-ready
  :config
  (advice-add #'Man-notify-when-ready :override #'display-buffer)
  (autoload 'ansi-osc-apply-on-region "ansi-osc")
  (defun lina-man-render-hyperlinks ()
    (let ((inhibit-read-only t))
      (ansi-osc-apply-on-region (point-min) (point-max))))
  :hook (Man-cooked-hook . lina-man-render-hyperlinks)
  :bind ("C-x m" . man))

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

(use-package windmove
  :ensure nil
  :bind
  ("C-x w <left>" . windmove-swap-states-left)
  ("C-x w <right>" . windmove-swap-states-right)
  ("C-x w <up>" . windmove-swap-states-up)
  ("C-x w <down>" . windmove-swap-states-down)
  (:repeat-map lina-windmove-repeat-map
               ("<left>" . windmove-swap-states-left)
               ("<right>" . windmove-swap-states-right)
               ("<up>" . windmove-swap-states-up)
               ("<down>" . windmove-swap-states-down)))

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
  (defun lina-flymake-diagnostics-hook ()
    (setq-local truncate-lines nil))
  :hook ((flymake-mode-hook . lina-flymake-hook)
         ((flymake-diagnostics-buffer-mode-hook
           flymake-project-diagnostics-mode-hook)
          .
          lina-flymake-diagnostics-hook)
         ((sh-base-mode-hook python-base-mode-hook) . flymake-mode)))

(use-package display-line-numbers
  :ensure nil
  :custom
  ((display-line-numbers-grow-only t)
   (display-line-numbers-width 4))
  :hook ((prog-mode-hook
          markdown-ts-mode-hook
          conf-mode-hook
          yaml-mode-hook
          yaml-ts-mode-hook)
         .
         display-line-numbers-mode))

(use-package goto-addr
  :ensure nil
  :hook ((prog-mode-hook . goto-address-prog-mode)
         (text-mode-hook . goto-address-mode)))

(use-package whitespace
  :ensure nil
  :custom (whitespace-style '(face
                              tabs
                              ;; spaces
                              ;; space-mark
                              trailing
                              lines
                              space-before-tab
                              ;; newline
                              ;; newline-mark
                              indentation
                              empty
                              space-after-tab
                              tab-mark
                              missing-newline-at-eof)))

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
  ("C-x F" . find-function)
  ("C-x L" . find-library)
  ("C-x V" . find-variable)
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

(use-package speedbar
  :ensure nil
  :custom ((speedbar-prefer-window t)
           (speedbar-show-unknown-files t)
           (speedbar-window-default-width 30)
           (speedbar-hide-button-brackets-flag t)))

;;; built-in externals

(use-package tramp
  :functions tramp-recentf-cleanup tramp-recentf-cleanup-all tramp-enable-method
  :ensure nil
  :demand t
  :custom
  (tramp-show-ad-hoc-proxies t)
  (tramp-remote-process-environment '("ENV="
                                      "TMOUT=0"
                                      "CDPATH="
                                      "HISTORY="
                                      "MAIL="
                                      "MAILCHECK="
                                      "MAILPATH="
                                      "PAGER=cat"
                                      "autocorrect="
                                      "correct="))
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
  :functions shell@remote
  :custom
  (shell-kill-buffer-on-exit nil)
  :config
  (define-advice shell (:around (fn &optional buffer file-name) remote)
    (funcall fn buffer (if (eq system-type 'windows-nt)
                           file-name
                         (or file-name
                             (executable-find "zsh" t)
                             "/bin/bash"))))
  (defun lina-shell-hook ()
    (setq-local comint-process-echoes t
                pcomplete-termination-string ""))
  :hook (shell-mode-hook . lina-shell-hook)
  :bind (:map shell-mode-map
              ("C-c C-u" . universal-argument)))

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

;;; third-party integrations

(use-package dumb-jump
  :ensure t
  :custom
  ((dumb-jump-prefer-searcher 'rg))
  :init
  (setq-default xref-backend-functions '(dumb-jump-xref-activate)))

(use-package magit
  :ensure t
  :preface
  (setq-default magit-define-global-key-bindings nil)
  :custom
  (magit-display-buffer-function #'display-buffer)
  (magit-commit-show-diff nil)
  (magit-pull-or-fetch t)
  :functions (magit-clone-read-args
              magit-branch-read-args
              lina/vertico-preselect-around)
  :config
  (defun lina/vertico-preselect-around (func &rest args)
    (let ((vertico-preselect 'prompt))
      (apply func args)))
  (advice-add #'magit-clone-read-args :around #'lina/vertico-preselect-around)
  (advice-add #'magit-branch-read-args :around #'lina/vertico-preselect-around)
  :bind
  ("C-x g" . magit-dispatch)
  (:map magit-mode-map
        ("f" . magit-pull))
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
  :ensure t
  :pin melpa
  :custom
  (envrc-global-mode t))

(use-package with-editor
  :ensure t
  :pin nongnu-devel
  :config
  (defun lina-ghostel-pre-spawn-hook ()
    (when (fboundp 'with-editor--setup)
      (let ((with-editor--envvar "EDITOR"))
        (with-editor--setup))))
  :hook ((ghostel-pre-spawn-hook . lina-ghostel-pre-spawn-hook)
         (eshell-mode-hook . with-editor-export-editor)))

(use-package delight
  :ensure t)

(use-package ghostel
  :functions ghostel-module-compile@no-colour
  :defines ghostel-semi-char-mode-map
  :autoload (ghostel-send-string)
  :custom
  ((ghostel-shell (or (executable-find "zsh") "/bin/bash"))
   (ghostel-term "xterm-256color")
   (ghostel-module-auto-install nil)
   (ghostel-module-compile-command "zig build --color off --prefix %s -Doptimize=ReleaseFast -Dcpu=baseline")
   (ghostel-keymap-exceptions '("C-c" "C-x" "C-h" "M-x" "M-:" "M-&" "M-!"))
   (ghostel-point-leave-input-mode nil)
   (ghostel-buffer-name-function #'ghostel-buffer-name-by-title))
  :bind
  (:map ghostel-mode-map
        ("C-c C-u" . universal-argument))
  (:map project-prefix-map
        ("t" . ghostel-project-list-buffers)))

;;; third-party minor modes

(use-package gcmh
  :ensure t
  :commands (gcmh-mode)
  :init
  (defun lina-turn-on-gcmh ()
    (if (string= (system-name) "melee")
        (setq gc-cons-threshold (eval-and-compile
                                  (* (expt 1024 2) 16)))
      (gcmh-mode)))
  :delight gcmh-mode
  :hook (emacs-startup-hook . lina-turn-on-gcmh))

(use-package sly
  :custom ((inferior-lisp-program "sbcl")
           (sly-complete-symbol-function #'sly-simple-completions)
           (sly-symbol-completion-mode nil))
  :config
  (setf (alist-get common-lisp-hyperspec-root browse-url-handlers
                   nil nil #'string-equal)
        #'eww-browse-url)
  :bind (:map sly-mode-map
              ("C-c f" . sly-describe-function)
              ("C-c v" . sly-describe-symbol)
              ("C-c i" . sly-documentation-lookup)
              ("C-c C-p" . sly-macroexpand-1)))

(use-package yasnippet
  :ensure t
  :preface
  (setq yas-alias-to-yas/prefix-p nil)
  :custom
  ((yas-new-snippet-default "\
# -*- mode: snippet -*-
# key: ${2:${1:$(yas--key-from-desc yas-text)}}
# --
$0`(yas-escape-text yas-selected-text)`")))

;;; built-in virtual major modes

(use-package dired
  :ensure nil
  :custom
  (delete-by-moving-to-trash t)
  (dired-auto-revert-buffer t)
  (dired-clean-confirm-killing-deleted-buffers nil)
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-listing-switches "-ahlDFZ --group-directories-first")
  (dired-recursive-deletes 'always)
  :init
  (setenv "LC_COLLATE" "C")
  :hook
  (dired-mode-hook . dired-hide-details-mode)
  :bind
  ("C-x d" . dired-jump)
  ("C-x C-d" . dired)
  (:map dired-mode-map
        ([remap dired-mouse-find-file-other-window]
         . dired-mouse-find-file)))

(use-package ibuffer
  :ensure nil
  :custom ((ibuffer-default-sorting-mode 'filename/process)))

(use-package eww
  :ensure nil
  :custom ((eww-auto-rename-buffer 'title)
           (eww-header-line-format nil)))

;;; built-in language major modes

(use-package prog-mode
  :ensure nil
  :config
  (defun lina-prog-mode-hook ()
    (if (fboundp 'delete-trailing-whitespace-mode)
        (delete-trailing-whitespace-mode t)
      (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))
    (setq-local completion-styles '(emacs22 partial-completion)))
  :hook ((prog-mode-hook yaml-mode-hook) . lina-prog-mode-hook)
  :bind (:map prog-mode-map
              ("DEL" . backward-delete-char-untabify)))

(use-package text-mode
  :ensure nil
  :config
  (defun lina-text-mode-hook ()
    (unless (memq major-mode '(yaml-mode yaml-ts-mode))
      (setq-local cursor-type 'bar)
      (auto-fill-mode)
      (visual-line-mode)))
  :hook (text-mode-hook . lina-text-mode-hook))

(use-package treesit
  :ensure nil
  :defines treesit-language-source-alist
  :custom
  (treesit-auto-install-grammar 'always)
  (treesit-enabled-modes '(bash-ts-mode
                           js-ts-mode
                           json-ts-mode
                           typescript-ts-mode
                           tsx-ts-mode
                           python-ts-mode
                           markdown-ts-mode))
  (treesit-font-lock-level 4))

(use-package conf-mode
  :ensure nil
  :mode
  (((rx "/containers/systemd/" (+ nonl) "."
        (or "container"
            "volume"
            "service"
            "pod"
            "image"
            "build"
            "network")
        eos)
    .
    conf-desktop-mode)
   ((rx "/isyncrc" eos) . conf-space-mode)
   ((rx ".ovpn" eos) . conf-space-mode)
   ((rx "/" (or "sysusers.d" "tmpfiles.d") "/" (+ nonl) ".conf" eos)
    .
    conf-space-mode)))

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
  ;; :vc (:url "https://git.sr.ht/~eshel/kubed" :rev "master")
  :bind
  ("C-c k" . kubed-transient))

;;; other files

(load "lina-window")
(load "lina-puni")
(load "lina-elisp")
(load "lina-mail")
(load "lina-eglot")
(load "lina-org")
(load "lina-c")
(load "lina-llm")
(load "lina-eshell")
(load "lina-fmt")
(load "lina-js")
(load "lina-yaml")

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
