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
  (backup-directory-alist (list (cons "." (locate-user-emacs-file "backups/"))))
  (blink-cursor-mode nil)
  (bookmark-save-flag 1)
  (bookmark-fringe-mark nil)
  (column-number-mode t)
  (confirm-kill-processes nil)
  (create-lockfiles nil)
  (css-indent-offset 2)
  (deactivate-mark nil)
  (delete-selection-mode t)
  (enable-recursive-minibuffers t)
  (frame-title-format "%b")
  (indent-tabs-mode nil)
  (indicate-empty-lines t)
  (initial-major-mode #'fundamental-mode)
  (inhibit-startup-screen t)
  (initial-scratch-message nil)
  (kill-whole-line t)
  (make-backup-files nil)
  (mouse-autoselect-window t)
  (native-comp-async-report-warnings-errors nil)
  (read-answer-short nil)
  (read-hide-char ?\u2022)   ; BULLET
  (repeat-mode t)
  (ring-bell-function #'ignore)
  (safe-local-variable-directories '("~/.emacs.d"))
  (savehist-mode t)
  (save-place-mode t)
  (sh-basic-offset 2)
  (trusted-content :all)                ; FIXME! THIS SHIT NEVER WORKS
  (use-dialog-box nil)
  (use-short-answers t)
  (vc-handled-backends nil)
  (warning-minimum-level :emergency)
  :hook (after-init . (lambda ()
                        (ignore-errors
                          (make-directory (locate-user-emacs-file "backups/")))))
  :bind
  (("<escape>" . exit-recursive-edit)
   ("C-k" . kill-whole-line)
   ("C-z" . undo)
   ("C-S-z" . undo-redo)
   ("M-;" . comment-line)
   ("M-i" . completion-at-point)
   ("M-," . pop-to-mark-command)
   ("C-," . pop-global-mark)
   ("C-x C-g" . keyboard-quit)
   ("C-x x" . revert-buffer-quick)
   ("C-x C-x" . revert-buffer-quick)
   ("C-x C-c" . delete-frame-or-kill-emacs)
   ("C-w" . c-w-dwim)
   ("C-x 2" . split-and-follow-vertically)
   ("C-x 3" . split-and-follow-horizontally)
   ("C-x 4" . display-buffer)
   ("M-<left>" . backward-word)
   ("M-<right>" . forward-word)
   ("C-<right>" . forward-sexp)
   ("C-<left>" . backward-sexp)
   ("M-u" . nil)
   (:map minibuffer-local-map
         ("C-u" . backward-kill-sentence))))

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
   (:map isearch-mode-map
         ("TAB" . isearch-toggle-symbol-&-regexp-)
         ("<tab>" . isearch-toggle-symbol-&-regexp-)
         ("ESC" . isearch-exit)
         ("<escape>" . isearch-exit))))

(use-package help
  :ensure nil
  :custom
  (help-window-select t)
  :bind
  (("C-h ," . customize-variable)
   ("C-h c" . describe-char)
   ("C-h g" . customize-group)
   ("C-h s" . info-lookup-symbol)
   ("C-h C-h" . nil)))

(use-package conf-mode
  :ensure nil
  :mode "\\.gitignore\\'"
  :mode ("\\.container\\'" . conf-desktop-mode)
  :mode ("\\.volume\\'" . conf-desktop-mode)
  :mode ("\\.network\\'" . conf-desktop-mode))

(use-package recentf
  :ensure nil
  :custom
  (recentf-mode t)
  (recentf-max-menu-items most-positive-fixnum)
  (recentf-max-saved-items 80)
  :bind
  ("C-x C-r" . recentf-open)
  ("C-r" . recentf-open))

(use-package mwheel
  :ensure nil
  :custom
  (scroll-conservatively 101)
  (scroll-step 1)
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

(use-package project
  :ensure nil
  :autoload project-root
  :functions my-project-prompt-dir
  :commands project-add-dir-local-variable find-flake
  :custom
  (project-vc-extra-root-markers '(".git"
                                   ".project"
                                   "pom.xml"
                                   "Cargo.toml"
                                   "flake.nix"
                                   ".devcontainer.json"
                                   ".devcontainer/"))
  (project-switch-use-entire-map t)
  :config
  (defun my-project-prompt-dir ()
    (if current-prefix-arg
        (read-directory-name "Select directory: " nil nil t)
      (or
       (and-let* ((project (project-current))) (project-root project))
       default-directory)))
  (defun project-add-dir-local-variable ()
    (interactive)
    (let ((default-directory (project-root (project-current))))
      (call-interactively #'add-dir-local-variable)))
  :bind
  ("C-x p d" . project-dired)
  ("C-x p s" . project-eshell))

(use-package compile
  :ensure nil
  :custom
  (compilation-always-kill t)
  (compilation-ask-about-save nil)
  (compilation-scroll-output 'first-error)
  (compile-command "make "))

(use-package flymake
  :ensure nil
  :hook
  (flymake-diagnostics-buffer-mode . visual-line-mode)
  ((sh-base-mode python-mode) . flymake-mode)
  :bind (:map project-prefix-map
              ("q" . flymake-show-project-diagnostics)))

(use-package eglot
  :ensure t
  :custom
  (eglot-ignored-server-capabilities '(:inlayHintProvider))
  (eglot-report-progress nil)
  :config
  (setq-default eglot-server-programs
                (list
                 (cons 'nix-mode (eglot-alternatives
                                  '("nil" "rnix-lsp" "nixd")))
                 '(dockerfile-ts-mode "docker-langserver" "--stdio")
                 '(go-ts-mode "gopls")
                 (list 'rust-mode "rust-analyzer")
                 '(js-mode "typescript-language-server" "--stdio")))
  (defun my-eglot-hook ()
    (setq-local eldoc-echo-area-use-multiline-p (eglot-managed-p))
    (when (memq major-mode '(dockerfile-ts-mode))
      (add-hook 'before-save-hook #'eglot-format-buffer nil 'local)))
  :hook (eglot-managed-mode . my-eglot-hook)
  :bind (:map eglot-mode-map
              ("C-l" . eglot-format)
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
  ("C-x p g" . consult-grep-)
  ("C-x p f" . consult-find-)
  ("C-x b" . consult-buffer)
  ("C-h i" . consult-info))

(use-package embark-consult :ensure t)

(use-package embark
  :ensure t
  :custom
  (embark-indicators '(embark-minimal-indicator
                       embark-highlight-indicator
                       embark-isearch-highlight-indicator))
  (embark-cycle-key "<tab>")
  (prefix-help-command #'embark-prefix-help-command)
  :config
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
        ("1" . eglot-code-actions))
  (:map minibuffer-mode-map
        ("C-S-x" . embark-export)))

(use-package corfu
  :ensure t
  :custom
  (corfu-quit-no-match nil)
  (global-corfu-mode t)
  (global-corfu-modes '((not inferior-python-mode) t)))

(use-package cape
  :ensure t
  :config
  (defun my-elisp-cape-hook ()
    (add-hook 'completion-at-point-functions
              #'cape-elisp-symbol -1 t))
  (defun my-cape-dabbrev-hook ()
    (add-hook 'completion-at-point-functions
              #'cape-dabbrev -1 t))
  :hook (((emacs-lisp-mode inferior-emacs-lisp-mode)
          .
          my-elisp-cape-hook)
         ((hcl-mode yaml-mode) . my-cape-dabbrev-hook))
  ;; :bind (:map org-mode-map
  ;;             ("C-c ?" . cape-tex))
  )

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
    (my-elisp-view-hook)
    ;; (unless buffer-read-only
    ;;   (let ((auto-insert-query nil))
    ;;     (auto-insert)))
    )
  (define-advice elisp-enable-lexical-binding
      (:around (fun &rest _) no-ask)
    (save-excursion
      (funcall fun nil)))
  :hook (emacs-lisp-mode . my-elisp-mode-hook)
  :bind (:map emacs-lisp-mode-map
              (";" . comment-dwim)
              ("C-c C-c" . elisp-eval-region-or-buffer))
  :mode ("\\.dir-locals\\(?:-2\\)?\\.el\\'" . emacs-lisp-mode))

(use-package erefactor
  :ensure t
  :bind (:map emacs-lisp-mode-map
              ("<f2>" . erefactor-rename-symbol-in-buffer)))

(use-package autoinsert
  :ensure nil
  :custom
  (auto-insert-directory (locate-user-emacs-file "auto-insert/"))
  (auto-insert-alist `((,(rx "." (or "tex" "ltx") string-end) . "latex")
                       ("\\.el\\'" . ,(lambda ()
                                        (elisp-enable-lexical-binding))))))

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
    (setq-local whitespace-style '(tab-mark))
    (whitespace-mode))
  (setq-mode-local makefile-mode
                   indent-tabs-mode t
                   indent-line-function #'indent-makefile
                   tab-always-indent t)
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
                  (yaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml")))
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

(use-package puni
  :ensure t
  :config
  (defun my-puni-c-w-dwim (&optional prefix)
    "if region is active delete whats in the region. otherwise, delete the
preceding sexp"
    (interactive "p")
    (if (use-region-p)
        (puni-kill-region)
      (backward-kill-sexp prefix)))
  (defun my-puni-kill-whole-line ()
    "delete the whole ass line the point is on"
    (interactive)
    (let ((kill-whole-line t))
      (move-beginning-of-line nil)
      (puni-kill-line)))
  :hook
  (puni-mode . electric-pair-local-mode)
  (prog-mode . puni-mode)
  :bind
  (:map puni-mode-map
        ("C-9" . puni-wrap-round)
        ("C-<backspace>" . puni-backward-kill-line)
        ("C-k" . my-puni-kill-whole-line)
        ("C-w" . my-puni-c-w-dwim)
        ("C-c DEL" . puni-force-delete)
        ("C-c r" . puni-raise)
        ("C-c ." . puni-slurp-forward)
        ("C-c s" . puni-splice))
  (:repeat-map my-puni-repeat-map
               ("." . puni-slurp-forward)))

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

(use-package nix-mode
  :ensure t
  :config
  (defun my-nix-hook ()
    (add-hook 'before-save-hook #'nix-format-before-save nil 'local))
  (defun lina-nix-repl-hook ()
    (when (fboundp 'corfu-mode)
      (corfu-mode -1)))
  :hook
  (nix-mode . my-nix-hook)
  (nix-repl-mode . lina-nix-repl-hook))

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
  (explicit-shell-file-name "zsh")
  :hook (shell-command-mode . view-mode))

(use-package dired
  :ensure nil
  :custom
  (delete-by-moving-to-trash t)
  (dired-recursive-deletes 'always)
  (dired-clean-confirm-killing-deleted-buffers nil)
  (dired-listing-switches "-aFlh")
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
  :custom (transient-display-buffer-action
           '(display-buffer-at-bottom
             (dedicated . t)
             (inhibit-same-window . t))))

(use-package magit
  :ensure t
  :custom
  (magit-display-buffer-function #'display-buffer)
  (magit-commit-show-diff nil)
  (magit-patch-save-arguments nil)
  :init
  (magit-auto-revert-mode t)
  :bind
  ("C-x g" . magit))

(defconst my-linux-font '(:family "Iosevka Nerd Font" :height 105))

(use-package faces
  :ensure nil
  :custom-face
  (default ((((type x pgtk)) ,my-linux-font)))
  (fixed-pitch ((((type x pgtk)) ,my-linux-font)))
  (tab-line-tab-current ((t (:inherit unspecified)))))

(use-package desktop
  :ensure nil
  :custom
  (desktop-save-mode nil)
  (desktop-save t)
  (desktop-dir-name user-emacs-directory)
  (desktop-load-locked-desktop 'check-pid))

(use-package eshell
  :ensure nil
  :custom
  (eshell-confine-point-to-input t)
  (eshell-prefer-lisp-functions t)
  (eshell-ls-initial-args "-FHh")
  :bind
  ;; (:package esh-mode :map eshell-mode-map
  ;;           ("C-u" . eshell-kill-input))
  (:package esh-mode :map eshell-mode-map
            ("C-d" . eshell-send-eof-to-process)
            ("C-c" . eshell-interrupt-process)))

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
    ;; (setq-local indent-line-function #'tab-to-tab-stop
    ;;             tab-stop-list '(4 8))
    (setq-local indent-line-function #'indent-relative)
    )
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

(use-package prettier-js
  :commands prettier-js)

(use-package astro-ts-mode
  :ensure t
  :config
  (defun lina-astro-hook ()
    (display-line-numbers-mode))
  :hook (astro-ts-mode . lina-astro-hook)
  :mode "\\.astro\\'")

(use-package css-mode
  :bind (:map css-mode-map
              ("C-c C-c" . recompile)))

(use-package gptel
  :ensure t
  :custom
  (gptel-log-level 'debug)
  :config
  (setq gptel-backend (gptel-make-openai "ramalama"
                        :protocol "http"
                        :host "127.0.0.1:8080"
                        :models '(deepseek)
                        :stream t)))

(use-package eat
  :ensure t
  :custom
  (eat-term-name "xterm-256color")
  :hook
  (eshell-first-time-mode . eat-eshell-mode)
  :bind
  (:map eat-mode-map
        ("C-u" . eat-self-input))
  (:map eat-semi-char-mode-map
        ("M-w" . nil))
  (:map project-prefix-map
        ("t" . eat-project)))

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
  :custom
  (xref-backend-functions (list
                           #'dumb-jump-xref-activate
                           #'etags--xref-backend)))

(use-package marginalia
  :ensure t
  :custom (marginalia-mode t))

(use-package orderless
  :ensure t
  :demand t
  :custom (orderless-component-separator " +\\|[-/]")
  :config (setopt completion-styles '(orderless basic)))

(use-package diff-hl
  :ensure t)

(load "init/window")
(load "init/org")
(load "init/auctex")
(require 'lina-js)
