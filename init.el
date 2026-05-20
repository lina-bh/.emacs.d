;; -*- lexical-binding: t; -*-
(load (setq custom-file (locate-user-emacs-file "custom.el")) t)

(add-to-list 'load-path (locate-user-emacs-file "lisp/") t)

(setq-default
 package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                    ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                    ("melpa" . "https://melpa.org/packages/"))
 package-install-upgrade-built-in t
 use-package-always-defer t)

(load "funs/autoloads")

(package-install 'delight)
(autoload 'setq-mode-local "mode-local")

(defun split-and-follow-vertically ()
  (interactive)
  (let ((window (split-window-below)))
    (select-window window)))

(defun split-and-follow-horizontally ()
  (interactive)
  (let ((window (split-window-right)))
    (select-window window)))

(defun delete-frame-or-kill-emacs ()
  (interactive)
  (condition-case nil
      (call-interactively #'delete-frame)
    (error
     (call-interactively #'save-buffers-kill-emacs))))

(defun c-w-dwim (&optional prefix)
  (interactive "p")
  (if (use-region-p)
      (kill-region (region-beginning)
                   (region-end))
    (backward-kill-word (or prefix 1))))

(use-package emacs
  :ensure nil
  :custom
  (auto-save-default nil)
  (blink-cursor-mode nil)
  (bookmark-save-flag 1)
  (column-number-mode t)
  (confirm-kill-processes nil)
  (create-lockfiles nil)
  (delete-selection-mode t)
  (enable-recursive-minibuffers t)
  (help-window-select t)
  (indent-tabs-mode nil)
  (indicate-empty-lines t)
  (inhibit-startup-screen t)
  (initial-scratch-message nil)
  (kill-whole-line t)
  (make-backup-files nil)
  (mouse-autoselect-window t)
  (repeat-mode t)
  (ring-bell-function #'ignore)
  (savehist-mode t)
  (save-interprogram-paste-before-kill t)
  (save-place-mode t)
  (scroll-conservatively 101)
  (scroll-step 1)
  (sh-basic-offset 2)
  (use-dialog-box nil)
  (use-short-answers t)
  (vc-handled-backends '(Git))
  (warning-minimum-level :emergency)
  :bind
  ("C-k" . kill-whole-line)
  ("C-z" . undo)
  ("C-S-z" . undo-redo)
  ("C-S-c" . kill-ring-save)
  ("C-S-v" . yank)
  ("M-;" . comment-line)
  ("M-i" . completion-at-point)
  ("M-," . pop-to-mark-command)
  ("C-," . pop-global-mark)
  ("C-x C-g" . keyboard-quit)
  ("C-x x" . revert-buffer-quick)
  ("C-x C-c" . delete-frame)
  ("C-w" . c-w-dwim)
  ("C-x 2" . split-and-follow-vertically)
  ("C-x 3" . split-and-follow-horizontally)
  ("M-u" . nil)
  (:map minibuffer-local-map
        ("C-u" . backward-kill-sentence))
  (:map help-map
        ("g" . customize-group-other-window)))

(use-package isearch
  :ensure nil
  :config
  (defun isearch-toggle-symbol-&-regexp- ()
    "Toggle between symbol and regexp searching."
    (interactive)
    (if (not isearch-mode)
        (message "can be called in isearch-mode")
      (if (not isearch-regexp)
          (isearch-toggle-regexp)
        (isearch-toggle-symbol))))
  :bind
  (("C-s" . isearch-forward-regexp)
   ("C-r" . isearch-backward-regexp)
   (:map isearch-mode-map
         ("TAB" . isearch-toggle-symbol-&-regexp-)
         ("ESC" . isearch-exit))))

(use-package conf-mode
  :ensure nil
  :mode
  "\\.gitignore\\'"
  ((rx "." (or "container" "volume" "network" "service") eos) . conf-desktop-mode))

(use-package recentf
  :ensure nil
  :custom
  (recentf-mode t)
  (recentf-max-saved-items 80))

(use-package mwheel
  :ensure nil
  :custom
  (mouse-wheel-progressive-speed t)
  (mouse-wheel-scroll-amount '(1 ((shift) . 1)))
  (mouse-wheel-scroll-amount-horizontal 1)
  (mouse-wheel-tilt-scroll t)
  (mouse-wheel-flip-direction (eq window-system 'ns)))

(use-package xref
  :ensure nil
  :custom
  (xref-prompt-for-identifier nil)
  :config
  (define-advice xref-matches-in-files
      (:around (xref-matches-in-files &rest args) ripgrep)
    (let ((xref-search-program (or (and (executable-find "rg" t)
                                        'ripgrep)
                                   'grep)))
      (apply xref-matches-in-files args)))
  :bind
  ("M-/" . xref-find-definitions))

(use-package flymake
  :ensure t
  :pin gnu
  :hook
  (flymake-diagnostics-buffer-mode . visual-line-mode)
  ((sh-base-mode python-mode) . flymake-mode))

(use-package eglot
  :ensure t
  :custom
  (eglot-ignored-server-capabilities '(:inlayHintProvider))
  (eglot-report-progress nil)
  :config
  (defun my-eglot-hook ()
    (setq-local eldoc-echo-area-use-multiline-p (eglot-managed-p)))
  :hook (eglot-managed-mode . my-eglot-hook)
  :bind (:map eglot-mode-map
              ("<f2>" . eglot-rename)))

(use-package consult
  :ensure t
  :custom
  (consult-async-split-style nil)
  (consult-find-args "find .")
  (consult-line-start-from-top t)
  (consult-preview-allowed-hooks '())
  (consult-preview-max-count 0)
  (completion-in-region-function #'consult-completion-in-region)
  (xref-show-definitions-function #'consult-xref)
  (xref-show-xrefs-function #'consult-xref)
  :config
  (defun consult-grep- ()
    (interactive)
    (call-interactively
     (or
      (and (executable-find "rg" 'remote)
           #'consult-ripgrep)
      #'consult-grep)))
  (defun consult-find- ()
    (interactive)
    (call-interactively
     (or
      (and (or (executable-find "fd" 'remote)
               (executable-find "fdfind" 'remote))
           #'consult-fd)
      #'consult-find)))
  :bind
  ("M-s" . consult-line)
  ("M-g" . consult-imenu)
  ("M-y" . consult-yank-pop)
  (:map ctl-x-map
        ("C-r" . consult-recent-file))
  (:map project-prefix-map
        ("g" . consult-grep-)
        ("f" . consult-find-))
  (:map help-map
        ("b" . consult-mode-command)
        ("i" . consult-info)))

(use-package embark-consult :ensure t)

(use-package embark
  :ensure t
  :custom
  (embark-indicators '(embark-minimal-indicator
                       embark-highlight-indicator
                       embark-isearch-highlight-indicator))
  (embark-cycle-key "TAB")
  (prefix-help-command #'embark-prefix-help-command)
  :config
  ;; (delete 'embark-target-flymake-at-point embark-target-finders)
  (setf (alist-get 'identifier embark-default-action-overrides)
        #'xref-find-definitions)
  (unless (assq 'eglot-code-actions embark-target-injection-hooks)
    (push '(eglot-code-actions embark--ignore-target)
          embark-target-injection-hooks))
  :bind
  ("C-." . embark-act)
  ("M-." . embark-dwim))

(use-package corfu
  :ensure t
  :custom
  (global-corfu-mode t)
  (global-corfu-modes '((not comint-mode) t)))

(use-package cape
  :ensure t
  :config
  (defun my-elisp-cape-hook ()
    (add-hook 'completion-at-point-functions
              #'cape-elisp-symbol -1 t))
  :hook (((emacs-lisp-mode inferior-emacs-lisp-mode)
          .
          my-elisp-cape-hook)))

(use-package tab-line
  :ensure nil
  :custom
  (global-tab-line-mode t)
  (tab-line-new-button-show nil)
  (tab-line-close-button-show nil)
  (tab-line-switch-cycling nil)
  :custom-face
  (tab-line ((t (:height unspecified))))
  :config
  (define-advice tab-line-select-tab-buffer
      (:around (fun &rest args) dedicated)
    (let ((dedicated (window-dedicated-p)))
      (apply fun args)
      (set-window-dedicated-p (selected-window) dedicated))))

(use-package gcmh
  :ensure t
  :demand t
  :config (gcmh-mode t)
  :delight)

(use-package modus-themes
  :ensure t
  :custom
  (modus-themes-variable-pitch-ui t)
  (modus-themes-mixed-fonts t)
  (modus-themes-prompts '(intense))
  (modus-themes-to-toggle '(modus-operandi modus-vivendi))
  (modus-themes-common-palette-overrides
   '((fringe unspecified)
     (bg-line-number-inactive unspecified)
     (bg-line-number-active unspecified)
     (fg-line-number-active fg-main)))
  :custom-face
  (modus-themes-ui-variable-pitch ((((type x pgtk))
                                    :family
                                    "Inter"
                                    :height
                                    102)))
  :config
  (defun my-modus-custom ()
    (modus-themes-with-colors
      (custom-set-faces
       `(mode-line ((,c (:inherit nil))))
       `(mode-line-inactive ((,c (:inherit nil))))
       `(tab-line-highlight ((,c (:inherit nil)))))))
  (defun my-load-modus ()
    (when (display-graphic-p)
      (load-theme 'modus-operandi t)))
  :hook (modus-themes-after-load-theme . my-modus-custom)
  :hook (after-init . my-load-modus))

(use-package display-fill-column-indicator
  :ensure nil
  :custom
  (display-fill-column-indicator-character ?\u2595) ;; RIGHT ONE EIGHT BLOCK
  :hook (prog-mode . display-fill-column-indicator-mode))

(use-package display-line-numbers
  :ensure nil
  :custom
  (display-line-numbers-grow-only t)
  (display-line-numbers-width-start 1000)
  :hook ((prog-mode conf-mode yaml-mode yaml-ts-mode) . display-line-numbers-mode))

(use-package paren
  :ensure nil
  :custom
  (show-paren-mode nil)
  :hook ((prog-mode conf-mode yaml-mode yaml-ts-mode) . show-paren-local-mode))

(use-package autorevert
  :ensure nil
  :custom
  (auto-revert-mode-text "")
  (auto-revert-remote-files t)
  (global-auto-revert-mode t))

(use-package prog-mode
  :ensure nil
  :config
  (setq-mode-local prog-mode
                   show-trailing-whitespace t
                   truncate-lines t))

(use-package elisp-mode
  :ensure nil
  :functions elisp-enable-lexical-binding my-elisp-view-hook
  :config
  (defun my-elisp-view-hook ()
    (and
     buffer-file-name
     (not (file-in-directory-p buffer-file-name
                               (locate-user-emacs-file "lisp/")))
     (seq-some (lambda (path)
                 (file-in-directory-p buffer-file-name path))
               load-path)
     (view-mode)))
  (defun my-elisp-mode-hook ()
    (setq-local elisp-flymake-byte-compile-load-path load-path)
    (add-hook 'after-save-hook #'check-parens nil 'local)
    (remove-hook 'flymake-diagnostic-functions #'elisp-flymake-checkdoc t)
    (my-elisp-view-hook))
  (define-advice elisp-enable-lexical-binding
      (:around (fun &rest _) no-ask)
    (save-excursion
      (funcall fun nil)))
  :hook (emacs-lisp-mode . my-elisp-mode-hook)
  :bind (:map emacs-lisp-mode-map
              ("C-c C-c" . elisp-eval-region-or-buffer))
  :mode ("\\.dir-locals\\(?:-2\\)?\\.el\\'" . emacs-lisp-mode))

(use-package erefactor
  :ensure t
  :bind (:map emacs-lisp-mode-map
              ("<f2>" . erefactor-rename-symbol-in-buffer)))

(use-package pp
  :ensure nil
  :config
  (define-advice pp-display-expression
      (:after (&rest args) readonly)
    (let ((out-buffer-name (cadr args)))
      (with-current-buffer out-buffer-name
        (view-mode)
        (flymake-mode -1))))
  :bind
  ("M-:" . pp-eval-expression)
  (:map emacs-lisp-mode-map
        ("C-x C-e" . pp-eval-last-sexp)
        ("C-c C-p" . pp-macroexpand-last-sexp)))

(use-package make-mode
  :ensure nil
  :commands indent-makefile
  :config
  (defun indent-makefile ()
    (interactive)
    (let (in-rule)
      (save-excursion
        (move-beginning-of-line 0)
        (setq in-rule (and
                       (not (bobp))
                       (seq-some #'looking-at '("\t" ".+:.*$"))))) ;".+:.*$"
      (if in-rule
          (insert "\t")
        nil)))
  (defun my-makefile-hook ()
    (setq-local whitespace-style '(tab-mark)
                indent-tabs-mode t
                indent-line-function #'indent-makefile
                tab-always-indent t)
    (whitespace-mode))
  :hook (makefile-mode . my-makefile-hook))

(use-package asm-mode
  :ensure nil
  :config
  (setq-mode-local asm-mode tab-width 2)
  (defun my-asm-mode-hook ()
    (when (boundp 'asm-comment-char)
      (local-unset-key (vector asm-comment-char)))
    (unbind-key ":" 'asm-mode-map))
  :hook (asm-mode . my-asm-mode-hook))

(use-package python
  :ensure nil
  :custom
  (python-flymake-command '("ruff"
                            "check"
                            "--quiet"
                            "--output-format=concise"
                            "--stdin-filename=stdin"))
  (python-indent-guess-indent-offset-verbose nil)
  (python-shell-dedicated 'project))

(use-package treesit
  :if (treesit-available-p)
  :commands treesit-install-all-grammars-
  :init
  (setq-default treesit-language-source-alist
                '((dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
                  (go "https://github.com/tree-sitter/tree-sitter-go" "master")
                  (rust "https://github.com/tree-sitter/tree-sitter-rust")
                  (json "https://github.com/tree-sitter/tree-sitter-json")
                  (bash "https://github.com/tree-sitter/tree-sitter-bash")
                  (html "https://github.com/tree-sitter/tree-sitter-html")
                  (astro "https://github.com/virchau13/tree-sitter-astro")
                  (yaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml")
                  (dart "https://github.com/UserNobody14/tree-sitter-dart")))
  :config
  (defun treesit-install-all-grammars- ()
    (interactive)
    (dolist (src treesit-language-source-alist)
      (let ((lang (car src)))
        (unless (treesit-language-available-p lang)
          (message "Installing %s..." lang)
          (treesit-install-language-grammar lang)))))
  :custom
  ;; java typescript tsx lua
  (major-mode-remap-alist '((sh-mode . bash-ts-mode)
                            (js-json-mode . json-ts-mode)))
  :mode
  ("\\.go\\'" . go-ts-mode))

(use-package smartparens
  :ensure t
  :init
  (require 'smartparens-config)
  :custom
  (sp-highlight-pair-overlay nil)
  (sp-escape-quotes-after-insert nil)
  :config
  (defun sp-c-w-dwim ()
    (interactive)
    (if (use-region-p)
        (sp-kill-region (region-beginning) (region-end))
      (sp-backward-kill-sexp)))
  (defun lina-show-smartparens-mode-hook ()
    (when show-smartparens-mode
      (show-paren-local-mode -1)))
  :hook
  ((sh-base-mode) . turn-on-smartparens-mode)
  (emacs-lisp-mode . turn-on-smartparens-strict-mode)
  (show-smartparens-mode . lina-show-smartparens-mode-hook)
  (smartparens-mode . show-smartparens-mode)
  :bind
  (:map smartparens-mode-map
        ("C-c ." . sp-forward-slurp-sexp)
        ("C-c s" . sp-splice-sexp)
        ("C-c r" . sp-raise-sexp)
        ("C-w" . sp-c-w-dwim)
        ("M-<up>" . sp-up-sexp)
        ("M-<down>" . sp-down-sexp)
        ("M-<left>" . sp-backward-sexp)
        ("M-<right>" . sp-forward-sexp))
  (:map emacs-lisp-mode-map
        (";" . sp-comment))
  (:repeat-map smartparens-repeat-map
               ("." . sp-forward-slurp-sexp)
               ("r" . sp-raise-sexp)))

(use-package yaml-mode
  :disabled t
  :config
  (defun my-yaml-insert-item ()
    (interactive)
    (newline-and-indent)
    (yaml-electric-backspace 1)
    (insert "- ")
    (indent-for-tab-command))
  (define-advice yaml-compute-indentation
      (:around (yaml-compute-indentation &rest args) no-indent-lists)
    (cl-letf*
        ((looking-at (symbol-function 'looking-at))
         ((symbol-function 'looking-at)
          (lambda (regexp &rest args)
            (unless (string= regexp yaml-nested-map-re)
              (apply looking-at (cons regexp args))))))
      (apply yaml-compute-indentation args)))
  :hook (yaml-mode . puni-mode)
  :bind (:map yaml-mode-map
              ("M-RET" . my-yaml-insert-item)))

(use-package hcl-mode
  :ensure t
  :config (setq-mode-local hcl-mode truncate-lines nil)
  :mode "\\.tf\\'")

(use-package nftables-mode
  :ensure t
  :config
  (setq-mode-local nftables-mode indent-tabs-mode t))

(use-package rust-mode
  :ensure t
  :defines rust-mode-map
  :custom
  (rust-mode-treesitter-derive t)
  (rust-format-on-save t)
  :init
  (setq-mode-local rust-mode format-mode-command #'eglot-format)
  :mode "\\.rs\\'"
  :bind (:map rust-mode-map
              ("C-c C-c" . rust-compile)
              ("C-c C-k" . rust-check)))

(use-package caddyfile-mode :ensure t)

(use-package aggressive-indent
  :ensure t
  :hook (emacs-lisp-mode . aggressive-indent-mode))

(use-package text-mode
  :ensure nil
  :hook (text-mode . visual-line-mode))

(use-package ispell
  :ensure nil
  :custom
  (ispell-program-name "hunspell")
  (ispell-dictionary "en_GB"))

(use-package flyspell
  :ensure nil
  :bind (:map flyspell-mode-map
              ("C-M-i" . nil)
              ([down-mouse-2] . nil)
              ([mouse-3] . flyspell-correct-word)))

(use-package face-remap
  :ensure nil
  :commands text-scale-mode turn-off-text-scale
  :config
  (defun turn-off-text-scale ()
    (interactive)
    (text-scale-mode -1))
  (defun my-variable-pitch-mode-hook ()
    (setq-local cursor-type
                (if (bound-and-true-p buffer-face-mode)
                    'bar
                  t)))
  :hook (buffer-face-mode . my-variable-pitch-mode-hook))

(use-package markdown-mode
  :ensure t
  :custom
  (markdown-command "pandoc -t html5")
  :bind (:map markdown-mode-map
              ("C-c C-c" . recompile)))

(use-package comint
  :ensure nil
  :functions comint-skip-input
  :custom
  (comint-prompt-read-only t)
  (comint-scroll-to-bottom-on-input t)
  :bind (:map comint-mode-map
              ("C-u" . comint-kill-input)
              ("<up>" . comint-previous-input)
              ("<down>" . comint-next-input)))

(use-package ielm
  :ensure nil
  :commands ielm-interrupt
  :config
  (defun ielm-interrupt ()
    (interactive)
    (comint-skip-input)
    (ielm-return))
  :bind (:map ielm-map ("C-c" . ielm-interrupt)))

(use-package shell
  :ensure nil
  :custom
  (async-shell-command-buffer 'new-buffer)
  (shell-command-prompt-show-cwd t)
  :hook (shell-command-mode . view-mode))

(use-package dired
  :ensure nil
  :custom
  (delete-by-moving-to-trash t)
  (dired-recursive-deletes 'always)
  (dired-clean-confirm-killing-deleted-buffers nil)
  (dired-listing-switches "-aFlh")
  (dired-kill-when-opening-new-dired-buffer t)
  :config
  (define-advice dired-post-do-command (:after (&rest _) unmark)
    (dired-unmark-all-marks))
  :hook (dired-mode . dired-hide-details-mode)
  :bind
  ("C-x d" . dired-jump)
  (:map dired-mode-map
        ("<mouse-2>" . dired-mouse-find-file)))

(use-package ange-ftp
  :ensure nil
  :custom
  (ange-ftp-default-user "anonymous")
  (ange-ftp-generate-anonymous-password "guest")
  (ange-ftp-try-passive-mode t))

(use-package tramp
  :ensure nil
  :custom
  (tramp-verbose 2)
  :init
  (tramp-enable-method 'distrobox))

(use-package man
  :ensure nil
  :config
  (define-advice Man-completion-table (:override (&rest _) empty)
    nil)
  (advice-add 'Man-notify-when-ready :override #'display-buffer))

(use-package finder
  :ensure nil
  :config
  (define-advice finder-commentary
      (:around (fun &rest args) no-shrink)
    (cl-letf
        (((symbol-function 'shrink-window-if-larger-than-buffer) #'ignore))
      (apply fun args))))

(use-package envrc
  :ensure t
  :custom
  (envrc-global-mode t)
  (envrc-debug t)
  (envrc-none-lighter nil)
  (envrc-show-summary-in-minibuffer nil)
  :init
  (defalias 'direnv-reload #'envrc-reload)
  (defalias 'direnv-allow #'envrc-allow))

(use-package transient
  :ensure t
  :custom
  (transient-display-buffer-action '(display-buffer-at-bottom
                                     (dedicated . t)
                                     (inhibit-same-window . t)))
  :config
  (transient-bind-q-to-quit))

(use-package magit
  :ensure t
  :custom
  (magit-display-buffer-function #'display-buffer)
  (magit-commit-show-diff nil)
  (magit-patch-save-arguments nil)
  :init
  (magit-auto-revert-mode t)
  :bind
  ("C-c g" . magit-file-dispatch)
  ("C-x g" . magit-dispatch))

(use-package faces
  :ensure nil
  :preface
  (defconst my-linux-font '(:family "Iosevka Nerd Font" :height 105))
  :custom-face
  (default ((((type x pgtk)) ,my-linux-font)))
  (fixed-pitch ((((type x pgtk)) ,my-linux-font)))
  (tab-line-tab-current ((t (:inherit unspecified)))))



(use-package eldoc
  :ensure nil
  :custom
  (eldoc-echo-area-use-multiline-p nil)
  (eldoc-minor-mode-string nil))

(use-package dockerfile-ts-mode
  :commands indent-dockerfile
  :config
  (defun indent-dockerfile ()
    (interactive)
    (let ((char (save-excursion
                  (forward-line -1)
                  (end-of-line)
                  (preceding-char))))
      (if (= char ?\\)
          (indent-to-column 4)
        'no-indent)))
  (defun lina-dockerfile-ts-mode-hook ()
    (setq-local indent-line-function #'indent-relative))
  :hook (dockerfile-ts-mode . lina-dockerfile-ts-mode-hook)
  :delight (dockerfile-ts-mode "Containerfile")
  :mode ("\\(?:Containerfile\\(?:\\..*\\)?\\|\\.[Cc]ontainerrfile\\)\\'"))

(use-package html-ts-mode
  :ensure nil
  :config
  (defun lina-html-ts-hook ()
    (display-line-numbers-mode))
  :hook (html-ts-mode . lina-html-ts-hook)
  :bind (:map html-mode-map
              ("C-c C-c" . recompile))
  :mode "\\.html\\'")

(setopt ert-debug-on-error t)

(use-package devcontainer
  :preface
  ;; (package-install-file (locate-user-emacs-file "lisp/devcontainer.el"))
  :load-path (lambda ()
               (locate-user-emacs-file "lisp/devcontainer.el"))
  :custom
  (devcontainer-engine 'podman)
  (devcontainer-dotfiles-repository "https://github.com/lina-bh/dotfiles.git"))

(use-package css-mode
  :custom
  (css-indent-offset 2)
  :bind (:map css-mode-map
              ("C-c C-c" . recompile)))

(use-package gptel
  :ensure t
  :custom
  (gptel-log-level 'debug)
  :config
  (setq gptel-backend
        (gptel-make-gemini "Gemini"
          :key (gptel-api-key-from-auth-source
                "generativelanguage.googleapis.com")
          :stream t)))

(use-package xt-mouse
  :ensure nil
  :init
  (unless (display-graphic-p)
    (xterm-mouse-mode t)))

(use-package vertico
  :ensure t
  :custom
  (vertico-mode t)
  (vertico-count-format nil)
  :bind
  (:map vertico-map
        ("DEL" . vertico-directory-delete-char)
        ("RET" . vertico-directory-enter)))

(use-package kubernetes
  :bind
  (:map kubernetes-overview-mode-map
        ("n" . kubernetes-set-namespace)))

(use-package dumb-jump
  :ensure t
  :init
  (setq-default xref-backend-functions (list
                                        #'dumb-jump-xref-activate)))

(use-package marginalia
  :ensure t
  :custom (marginalia-mode t))

(use-package orderless
  :ensure t
  :demand t
  :custom
  (completion-styles '(orderless basic)))

(use-package apheleia
  :ensure t
  :hook
  ((js-mode dart-mode) . apheleia-mode))

(use-package ibuffer
  :ensure nil
  :custom
  (ibuffer-default-sorting-mode 'major-mode)
  :bind
  (:map ctl-x-map
        ("C-b" . ibuffer))
  (:map ibuffer-name-map
        ("<mouse-1>" . ibuffer-mouse-visit-buffer)))

(use-package project
  :ensure nil
  :bind
  (:map project-prefix-map
        ("d" . project-dired)))

(use-package eshell
  :ensure nil
  :custom
  (eshell-confine-point-to-input t)
  (eshell-ls-initial-args "-FHh")
  (eshell-visual-subcommands '(("tofu" "console")))
  (eshell-complex-commands '("chmod"))
  :bind
  (:map project-prefix-map
        ("s" . eshell))
  (:package esh-mode :map eshell-mode-map
            ("C-d" . eshell-send-eof-to-process)
            ("C-c" . eshell-interrupt-process)
            ("C-u" . eshell-kill-input)))

;; (load "init/window")
(load "init/org")
(load "init/auctex")
(require 'lina-js)
