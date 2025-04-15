;; -*- lexical-binding: t; -*-
(require 'mode-local)
(add-to-list 'load-path (locate-user-emacs-file "lisp/"))

;; (setq-default package-archives
;;               '(("gnu" . "https://elpa.gnu.org/packages/")
;;                 ("nongnu" . "https://elpa.nongnu.org/nongnu/")
;;                 ("melpa" . "https://melpa.org/packages/")
;;                 ("gnu-devel" . "https://elpa.gnu.org/devel/")))
(setq-default package-archives nil)
(setq-default package-archive-priorities '(("gnu-devel" . -1))
              use-package-always-defer t)

(use-package delight
  :ensure t)

(use-package gcmh
  :ensure t
  :demand t
  :config (gcmh-mode t)
  :delight)

(load (setq custom-file (locate-user-emacs-file "custom.el")) t)

;; emacs core

(require 'my-funs)

(dolist (key '("M-u" "s-q" "<XF86Back>" "<XF86Forward>"))
  (unbind-key key global-map))

(setopt
 blink-cursor-mode nil
 bookmark-save-flag 1
 bookmark-fringe-mark nil
 column-number-mode t
 create-lockfiles nil
 css-indent-offset 2
 deactivate-mark nil
 delete-selection-mode t
 eldoc-echo-area-use-multiline-p nil
 eldoc-minor-mode-string nil
 enable-recursive-minibuffers t
 frame-title-format "%b"
 fill-column 80
 help-window-select t
 indent-tabs-mode nil
 indicate-empty-lines t
 inhibit-startup-screen t
 kill-whole-line t
 make-backup-files nil
 mouse-autoselect-window t
 native-comp-async-report-warnings-errors nil
 read-hide-char ?\u2022   ; BULLET
 repeat-mode t
 ring-bell-function #'ignore
 safe-local-variable-directories '("~/.emacs.d")
 save-place-mode t
 sh-basic-offset 2
 use-dialog-box nil
 use-short-answers t
 warning-minimum-level :error)

(bind-keys
 ("C-S-z" . undo-redo)
 ("C-k" . kill-whole-line)
 ("C-v" . yank)
 ("C-z" . undo)
 ("M-;" . comment-line)
 ("M-i" . completion-at-point)
 ("M-," . pop-to-mark-command)
 :map minibuffer-local-map
 ("C-u" . backward-kill-sentence)
 :map isearch-mode-map
 ("ESC" . isearch-exit)
 :map ctl-x-map
 ("C-g" . keyboard-quit)
 ("x" . revert-buffer-quick)
 :map help-map
 ("," . customize-variable)
 ("c" . describe-char)
 ("g" . customize-group)
 ("s" . info-lookup-symbol)
 ("C-f" . describe-face))

(add-hook 'after-init-hook
          (defun my-init-hook ()
            (setopt vc-handled-backends '(Git))
            (let ((backups-directory (locate-user-emacs-file "backups/")))
              (make-directory backups-directory t)
              (setopt auto-save-file-name-transforms
                      `((".*" ,backups-directory t))))))

(add-to-list 'auto-mode-alist '("\\.gitignore\\'" . conf-mode))

(use-package recentf
  :ensure nil
  :custom
  (recentf-mode t)
  (recentf-max-menu-items most-positive-fixnum)
  (recentf-max-saved-items 80)
  :bind
  (:map ctl-x-map
        ("C-r" . recentf-open)))

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

;; finding stuff

(use-package icomplete
  :ensure nil
  :custom
  (fido-vertical-mode t)
  (icomplete-matches-format "")
  (icomplete-show-matches-on-no-input t)
  (icomplete-compute-delay 0)
  :config
  (defun my-icomplete-hook ()
    (setq-local
     truncate-lines t
     completion-auto-help nil
     completion-styles (default-value 'completion-styles)))
  :hook (icomplete-minibuffer-setup . my-icomplete-hook)
  :bind (:map icomplete-fido-mode-map
              ("TAB" . icomplete-fido-ret)
              ("<tab>" . icomplete-fido-ret)))
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
  :config
  (defun my-project-prompt-dir ()
    (if current-prefix-arg
        (read-directory-name "Select directory: " nil nil t)
      (project-prompt-project-dir)))
  (defun project-add-dir-local-variable ()
    (interactive)
    (let ((default-directory (project-root (project-current))))
      (call-interactively #'add-dir-local-variable)))
  (defun find-flake ()
    (interactive)
    (let ((default-directory (project-root (project-current t))))
      (find-file "./flake.nix")))
  :bind
  ("M-!" . project-shell-command)
  ("M-&" . project-async-shell-command)
  (:map project-prefix-map
        ("b" . project-list-buffers)
        ("d" . project-dired)))
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
  (eglot-events-buffer-config '(:size nil :format full))
  :config
  (defun my-eglot-hook ()
    (setq-local eldoc-echo-area-use-multiline-p
                (eglot-managed-p)))
  (setf (alist-get 'web-mode eglot-server-programs)
        (alist-get 'html-mode eglot-server-programs))
  :hook (eglot-managed-mode . my-eglot-hook)
  :bind (:map eglot-mode-map
              ("<f2>" . eglot-rename)))

;; movec

(use-package orderless
  :ensure t
  :demand t
  :custom (orderless-component-separator " +\\|[-/]")
  :config (setopt completion-styles '(orderless basic)))

(use-package marginalia
  :ensure t
  :custom (marginalia-mode t))

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
  (define-advice consult-grep
      (:around (consult-grep &rest args) ripgrep)
    (apply (or (and (executable-find "rg" 'remote) 'consult-ripgrep)
               consult-grep)
           args))
  (define-advice consult-find
      (:around (consult-find &rest args) fd)
    (apply (or (and (executable-find "fd" 'remote) 'consult-fd)
               consult-find)
           args))
  :bind
  ("M-g" . consult-imenu)
  ("M-y" . consult-yank-pop)
  (:map project-prefix-map
        ("g" . consult-grep)
        ("f" . consult-find))
  (:map ctl-x-map
        ("b" . consult-buffer)
        ("f" . consult-flymake))
  (:map help-map
        ("i" . consult-info)))

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
  (:map help-map
        ("b" . embark-bindings))
  (:map embark-identifier-map
        ("2" . eglot-rename)
        ("l" . eglot-code-actions))
  (:map icomplete-fido-mode-map
        ("C-." . embark-act))
  (:map icomplete-minibuffer-map
        ("C-." . embark-act)))

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

;; look and feel

(use-package window
  :ensure nil
  :preface
  (defun match-derived-modes (&rest modes)
    (let (xs)
      (dolist (mode modes)
        (push (cons 'derived-mode (intern (format "%s-mode" mode))) xs))
      xs))
  (defconst tray-buffer-criteria
    `(and (not (or ,@(match-derived-modes 'Info 'package-menu)
                   ,(rx bos "magit-diff")
                   "Shell Command"))
          (or ,@(match-derived-modes 'comint
                                     'special
                                     'term
                                     'flymake-project-diagnostics
                                     'flymake-diagnostics-buffer
                                     'apropos
                                     'compilation)
              (category . comint)
              ,(rx bos "*" (or "Finder"
                               ;; "Embark"
                               "TeX Help"
                               "Agenda Commands"
                               "Org Export Dispatcher"
                               "Org PDF LaTeX Output"))
              ,(rx (or "shell" "vterm" "eshell") "*")
              "COMMIT_EDITMSG")))
  :custom
  (display-buffer-base-action '((display-buffer-reuse-window
                                 display-buffer-in-previous-window
                                 display-buffer-reuse-mode-window
                                 display-buffer-use-least-recent-window)))
  (switch-to-buffer-obey-display-actions t)
  (switch-to-buffer-in-dedicated-window 'pop)
  (display-buffer-alist
   `((,(rx bos "*Pp")
      display-buffer-below-selected)
     (,(rx bos "*Customiz")
      (display-buffer-reuse-mode-window
       display-buffer-pop-up-window))
     (,(rx bos "*" (or "Man"
                       "info"))
      (display-buffer-reuse-mode-window
       display-buffer-same-window))
     (,tray-buffer-criteria
      display-buffer-in-side-window
      (window-height . ,(/ 1.0 3)))))
  :bind
  (:map ctl-x-map
        ("=" . balance-windows)
        ("q" . quit-window)
        ("[" . previous-buffer)))
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
    (load-theme 'modus-operandi t))
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
  :hook ((prog-mode conf-mode yaml-mode) . display-line-numbers-mode))

(use-package paren
  :ensure nil
  :custom
  (show-paren-mode nil)
  (show-paren-context-when-offscreen t)
  :hook ((prog-mode conf-mode yaml-mode) . show-paren-local-mode))

(use-package goto-addr
  :custom (goto-address-mail-regexp "")
  :hook
  (prog-mode . goto-address-prog-mode)
  ((text-mode vterm-mode) . goto-address-mode)
  :bind (:map goto-address-highlight-keymap
              ("<mouse-2>" . ignore)
              ("C-<down-mouse-1>" . goto-address-at-point)))

(use-package autorevert
  :ensure nil
  :custom
  (auto-revert-mode-text "")
  (global-auto-revert-mode t))

;; programming modes

(use-package prog-mode
  :ensure nil
  :config
  (setq-mode-local prog-mode
                   show-trailing-whitespace t
                   truncate-lines t))

(use-package elisp-mode
  :ensure nil
  :functions elisp-enable-lexical-binding
  :config
  (defun my-elisp-mode-hook ()
    (require 'autoinsert)
    (require 'seq)
    (add-hook 'after-save-hook #'check-parens nil 'local)
    (remove-hook 'flymake-diagnostic-functions #'elisp-flymake-checkdoc t)
    ;; make system lisp files read-only
    (let ((buffer-file-name (buffer-file-name)))
      (and
       buffer-file-name
       (not (file-in-directory-p buffer-file-name
                                 (locate-user-emacs-file "lisp/")))
       (seq-some (apply-partially #'file-in-directory-p buffer-file-name)
                 load-path)
       (view-mode)))
    (let ((auto-insert-query nil))
      (ignore auto-insert-query)
      (auto-insert)))
  :hook (emacs-lisp-mode . my-elisp-mode-hook)
  :bind (:map emacs-lisp-mode-map
              (";" . comment-dwim))
  :mode ("\\.dir-locals\\(?:-2\\)?\\.el\\'" . emacs-lisp-mode))

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
  (:map emacs-lisp-mode-map :prefix "C-c" :prefix-map my-elisp-C-c-map
        ("C-p" . pp-macroexpand-last-sexp)))

(use-package make-mode
  :ensure nil
  :commands indent-makefile
  :config
  (defun indent-makefile ()
    (interactive)
    (let (in-rule)
      (save-excursion
        (move-beginning-of-line 0)
        (when (and
               (not (bobp))
               (seq-some #'looking-at
                         '("\t" ".+:.*$")))
          (setq in-rule t)))
      (if in-rule
          (insert "\t")
        'noindent)))
  (setq-mode-local makefile-mode
                   indent-line-function #'indent-makefile
                   tab-always-indent t
                   whitespace-style '(face indentation tab-mark))
  :hook
  (makefile-mode . indent-tabs-mode)
  (makefile-mode . whitespace-mode))

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
  (python-shell-dedicated 'project)
  :config
  (defun my-python-mode-hook ()
    ;; use eglot instead maybe
    ;; (add-hook 'flymake-diagnostic-functions #'python-flymake nil t)
    )
  :hook (python-mode . my-python-mode-hook))

(use-package treesit-auto
  :if (treesit-available-p)
  :ensure t
  :autoload treesit-auto-add-to-auto-mode-alist
  :custom
  (global-treesit-auto-mode t)
  (treesit-auto-langs '(java
                        bash
                        json
                        typescript
                        tsx
                        rust
                        dockerfile
                        lua))
  :hook (after-init . treesit-auto-add-to-auto-mode-alist))

(use-package puni
  :ensure t
  :config
  (defun my-puni-c-w-dwim ()
    "if region is active delete whats in the region. otherwise, delete the
preceding sexp"
    (interactive)
    (if (use-region-p)
        (puni-kill-region)
      (backward-kill-sexp)))
  (defun my-puni-kill-whole-line ()
    "if `kill-whole-line' is true, delete the whole line the point is on"
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
        ("C-<left>" . puni-backward-sexp-or-up-list)
        ("C-<right>" . puni-forward-sexp-or-up-list)
        ("C-c <backspace>" . puni-force-delete))
  (:map puni-mode-map :prefix "C-c" :prefix-map my-puni-C-c-map
        ("r" . puni-raise)
        ("." . puni-slurp-forward)
        ("s" . puni-splice))
  (:repeat-map my-puni-repeat-map
               ("." . puni-slurp-forward)))

(use-package yaml-mode
  :ensure t
  :config
  (defun my-yaml-insert-item ()
    (interactive)
    (newline-and-indent)
    (yaml-electric-backspace 1)
    (insert "- ")
    (indent-for-tab-command))
  :hook (yaml-mode . puni-mode)
  :bind (:map yaml-mode-map
              ("M-RET" . my-yaml-insert-item)))

(use-package hcl-mode
  :ensure t
  :config
  (setq-mode-local hcl-mode truncate-lines nil)
  :mode "\\.tf\\'")

(use-package nix-mode
  :ensure t)

(use-package nftables-mode
  :ensure t
  :hook (nftables-mode . indent-tabs-mode))

(use-package rust-mode
  :ensure t
  :defines rust-mode-map
  :custom
  (rust-mode-treesitter-derive t)
  (rust-format-on-save t)
  :init
  (setq-mode-local rust-mode
                   format-mode-command #'eglot-format)
  :bind
  (:map rust-mode-map :prefix "C-c" :prefix-map my-rust-prefix-map
        ("C-c" . rust-compile)
        ("C-k" . rust-check))
  :mode ("\\.rs\\'" . rust-mode))

(use-package caddyfile-mode
  :ensure t)

;; formatters

(use-package aggressive-indent
  :ensure t
  :hook (emacs-lisp-mode . aggressive-indent-mode))

(use-package format-mode
  :disabled t
  :load-path "lisp/"
  :bind ("C-f" . format-mode))

(use-package reformatter
  :ensure t
  :config
  (define-advice reformatter--do-region (:around (fun &rest args)
                                                 quit-not-delete)
    (cl-letf (((symbol-function #'delete-windows-on)
               #'quit-windows-on))
      (apply fun args))))

(use-package nixfmt
  :disabled t
  :load-path "lisp/"
  :hook (nix-mode . nixfmt-on-save-mode))

(use-package ruff-format
  :disabled t
  :ensure t
  :init
  (setq-mode-local python-mode
                   format-mode-command #'ruff-format-buffer)
  :hook (python-mode . ruff-format-on-save-mode))

(use-package yq-fmt
  :disabled t
  :load-path "lisp/"
  :hook (yaml-mode . yq-fmt-on-save-mode))

(use-package tofu-fmt
  :disabled t
  :load-path "lisp/"
  :hook (hcl-mode . tofu-fmt-on-save-mode))

;; text modes

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

(use-package org
  :ensure t
  :custom
  (org-adapt-indentation nil)
  (org-agenda-window-setup 'current-window)
  (org-export-backends '(html latex))
  (org-export-with-smart-quotes t)
  (org-html-postamble nil)
  (org-link-descriptive nil)
  (org-refile-targets '((nil :maxlevel . 2)))
  (org-startup-folded 'show2levels)
  (org-support-shift-select t)
  ;; src
  (org-confirm-babel-evaluate nil)
  (org-src-preserve-indentation t)
  (org-src-window-setup 'plain)
  (org-src-lang-modes `(,@(and (fboundp #'LaTeX-mode)
                               (mapcar (lambda (lang)
                                         (cons lang 'LaTeX))
                                       '("latex" "beamer")))
                        ,@(and (treesit-language-available-p 'bash)
                               (mapcar (lambda (lang)
                                         (cons lang 'bash-ts))
                                       '("sh" "shell" "bash")))))
  (org-babel-load-languages '((emacs-lisp . t)
                              (python . t)
                              (R . t)
                              (shell . t)
                              ;; (latex . t)
                              ))
  ;; latex
  (org-latex-classes '(("article" "\\documentclass[a4paper,11pt]{article}"
                        ("\\section{%s}" . "\\section*{%s}")
                        ("\\subsection{%s}" . "\\subsection*{%s}")
                        ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                        ("\\paragraph{%s}" . "\\paragraph*{%s}")
                        ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))
  (org-latex-default-packages-alist
   '(("AUTO" "inputenc" t ("pdflatex"))
     ("T1" "fontenc" t ("pdflatex"))
     ("" "fontspec" t ("lualatex"))
     ("" "graphicx" t)
     ("" "amsmath" t)
     ("bookmarks=false,colorlinks=true,urlcolor=blue,linkcolor=,citecolor="
      "hyperref" nil)))
  (org-latex-packages-alist '(("margin=1in" "geometry")
                              ("" "lmodern")
                              ("" "minted")
                              ("british" "babel")))
  (org-format-latex-options '(
                              :foreground 'default
                              :background "Transparent"
                              :scale 2.0
                              :html-foreground "Black"
                              :html-background "Transparent"
                              :html-scale 1.0
                              :matchers ("begin" "$1" "$" "$$" "\\(" "\\["))
                            )
  (org-latex-src-block-backend 'minted)
  (org-latex-minted-options '(
                              ;; ("breaklines" . t)
                              ;; ("ignorelexererrors" . t)
                              ;; ("samepage" . t)
                              ))
  (org-latex-remove-logfiles nil)
  (org-latex-pdf-process
   '("%latex -interaction=nonstopmode -output-directory=%o -shell-escape \
   -draftmode %f"
     "%latex -interaction=batchmode -halt-on-error -shell-escape \
   -output-directory=%o %f")
   )
  :config
  (setq-mode-local org-mode line-spacing 0.2)
  (defun my-org-hook ()
    (variable-pitch-mode)
    (flyspell-mode)
    (face-remap-add-relative 'variable-pitch
                             :family "Liberation Serif"
                             :height 120))
  :hook
  ((org-mode org-agenda-mode) . my-org-hook)
  (org-babel-after-execute . org-redisplay-inline-images)
  :bind
  (:map mode-specific-map
        ("C-l" . org-store-link))
  (:map org-mode-map :prefix "C-c" :prefix-map my-org-prefix-map
        ("l" . org-latex-preview)
        ("p" . org-latex-export-to-pdf)
        ("t" . org-babel-tangle)
        ("s p" . org-latex-export-section-to-pdf)
        ("'" . org-edit-special)
        ("x" . org-babel-execute-buffer)
        ("C-c" . org-babel-execute-src-block))
  (:map org-mode-map
        ("C-t" . org-todo))
  (:map org-src-mode-map
        ("C-c C-c" . org-edit-src-exit)))

(use-package tex
  :ensure auctex
  :defines LaTeX-mode-map
  :custom
  (TeX-parse-self t)
  (TeX-auto-save t)
  (TeX-master 'shared)
  (TeX-save-query nil)
  (LaTeX-verbatim-environments
   '("verbatim"
     "verbatim*"
     "filecontents"
     "filecontents*"
     "minted"))
  (LaTeX-indent-environment-list
   '(("multicols" current-indentation) ("verbatim" current-indentation)
     ("verbatim*" current-indentation) ("filecontents" current-indentation)
     ("filecontents*" current-indentation) ("tabular" LaTeX-indent-tabular)
     ("tabular*" LaTeX-indent-tabular) ("array" LaTeX-indent-tabular)
     ("eqnarray" LaTeX-indent-tabular) ("eqnarray*" LaTeX-indent-tabular)
     ("align" LaTeX-indent-tabular) ("align*" LaTeX-indent-tabular)
     ("aligned" LaTeX-indent-tabular) ("alignat" LaTeX-indent-tabular)
     ("alignat*" LaTeX-indent-tabular) ("alignedat" LaTeX-indent-tabular)
     ("xalignat" LaTeX-indent-tabular) ("xalignat*" LaTeX-indent-tabular)
     ("xxalignat" LaTeX-indent-tabular) ("flalign" LaTeX-indent-tabular)
     ("flalign*" LaTeX-indent-tabular) ("split" LaTeX-indent-tabular)
     ("matrix" LaTeX-indent-tabular) ("pmatrix" LaTeX-indent-tabular)
     ("bmatrix" LaTeX-indent-tabular) ("Bmatrix" LaTeX-indent-tabular)
     ("vmatrix" LaTeX-indent-tabular) ("Vmatrix" LaTeX-indent-tabular)
     ("smallmatrix" LaTeX-indent-tabular) ("cases" LaTeX-indent-tabular)
     ("longtable" LaTeX-indent-tabular) ("longtable*" LaTeX-indent-tabular)
     ("matrix*" LaTeX-indent-tabular) ("pmatrix*" LaTeX-indent-tabular)
     ("bmatrix*" LaTeX-indent-tabular) ("Bmatrix*" LaTeX-indent-tabular)
     ("vmatrix*" LaTeX-indent-tabular) ("Vmatrix*" LaTeX-indent-tabular)
     ("smallmatrix*" LaTeX-indent-tabular) ("psmallmatrix" LaTeX-indent-tabular)
     ("psmallmatrix*" LaTeX-indent-tabular) ("bsmallmatrix" LaTeX-indent-tabular)
     ("bsmallmatrix*" LaTeX-indent-tabular) ("vsmallmatrix" LaTeX-indent-tabular)
     ("vsmallmatrix*" LaTeX-indent-tabular) ("Vsmallmatrix" LaTeX-indent-tabular)
     ("Vsmallmatrix*" LaTeX-indent-tabular) ("dcases" LaTeX-indent-tabular)
     ("dcases*" LaTeX-indent-tabular) ("rcases" LaTeX-indent-tabular)
     ("rcases*" LaTeX-indent-tabular) ("drcases" LaTeX-indent-tabular)
     ("drcases*" LaTeX-indent-tabular) ("cases*" LaTeX-indent-tabular)
     ("stabular" LaTeX-indent-tabular) ("stabular*" LaTeX-indent-tabular)
     ("supertabular" LaTeX-indent-tabular) ("supertabular*" LaTeX-indent-tabular)
     ("mpsupertabular" LaTeX-indent-tabular)
     ("mpsupertabular*" LaTeX-indent-tabular) ("tblr" LaTeX-indent-tabular)
     ("longtblr" LaTeX-indent-tabular) ("talltblr" LaTeX-indent-tabular)
     ("booktabs" LaTeX-indent-tabular) ("+array" LaTeX-indent-tabular)
     ("+matrix" LaTeX-indent-tabular) ("+bmatrix" LaTeX-indent-tabular)
     ("+Bmatrix" LaTeX-indent-tabular) ("+pmatrix" LaTeX-indent-tabular)
     ("+vmatrix" LaTeX-indent-tabular) ("+Vmatrix" LaTeX-indent-tabular)
     ("+cases" LaTeX-indent-tabular) ("tabularx" LaTeX-indent-tabular)
     ("tabulary" LaTeX-indent-tabular) ("xltabular" LaTeX-indent-tabular)
     ("xtabular" LaTeX-indent-tabular) ("xtabular*" LaTeX-indent-tabular)
     ("mpxtabular" LaTeX-indent-tabular) ("mpxtabular*" LaTeX-indent-tabular)
     ("displaymath") ("equation") ("picture") ("tabbing") ("gather") ("gather*")
     ("gathered") ("equation*") ("multline") ("multline*") ("macrocode")
     ("macrocode*")))
  (LaTeX-verbatim-macros-with-braces '("Sexpr"))
  (TeX-view-program-list '(("Okular"
                            ("org.kde.okular --unique %o"
                             (mode-io-correlate "#src:%n%a"))
                            "okular")))
  (TeX-view-program-selection `((output-pdf ,(cond
                                              ((eq system-type 'darwin)
                                               "Skim")
                                              ((eq system-type 'gnu/linux)
                                               "Okular")))))
  (LaTeX-section-hook '(LaTeX-section-heading
                        LaTeX-section-title
                        LaTeX-section-section))
  ;; (reftex-plug-into-AUCTeX t)
  :custom-face
  (font-latex-warning-face ((t (:weight normal))))
  :config
  (setq-mode-local LaTeX-mode TeX-command-force "LaTeX")
  :hook (TeX-mode . variable-pitch-mode)
  ;; :hook (TeX-mode . turn-on-reftex)
  :bind
  (:map TeX-mode-map
        ("M-=" . count-words)
        ("C-c C-c" . TeX-command-run-all)
        ("C-c C-a" . TeX-command-master)
        ("C-c C-f" . LaTeX-math-frac)
        ("C-f" . TeX-font)))

(use-package markdown-mode
  :ensure t)

;; applications

(use-package comint
  :ensure nil
  :functions comint-skip-input
  :custom
  (comint-prompt-read-only t)
  (comint-scroll-to-bottom-on-input t)
  :bind (:map comint-mode-map
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
  :bind ("C-c" . ielm-interrupt))

(use-package shell
  :ensure nil
  :custom
  (async-shell-command-buffer 'new-buffer)
  (shell-command-prompt-show-cwd t)
  (shell-kill-buffer-on-exit t))

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
  (:map ctl-x-map
        ("d" . dired-jump))
  (:map dired-mode-map
        ("<mouse-2>" . dired-mouse-find-file)))

(use-package ange-ftp
  :ensure nil
  :custom
  (ange-ftp-default-user "anonymous")
  (ange-ftp-generate-anonymous-password "guest")
  (ange-ftp-try-passive-mode t))

(use-package man
  :ensure nil
  :config
  (advice-add 'Man-completion-table :override #'ignore)
  (advice-add 'Man-notify-when-ready :override #'display-buffer))

(use-package finder
  :ensure nil
  :config
  (define-advice finder-commentary
      (:around (fun &rest args) no-shrink)
    (cl-letf
        (((symbol-function 'shrink-window-if-larger-than-buffer) #'ignore))
      (apply fun args))))

(use-package vterm
  :ensure t
  :bind
  (:map vterm-mode-map
        ("M-w" . nil)
        ("C-y" . nil)
        ("M-." . nil)
        ("C-u" . vterm--self-insert)
        ("C-c" . vterm--self-insert)
        ("C-t" . vterm-copy-mode))
  (:map vterm-copy-mode-map
        ("q" . vterm-copy-mode-done)))

(use-package project-vterm
  :load-path "lisp/"
  :bind (:map project-prefix-map
              ("s" . project-vterm)))

(use-package envrc
  :ensure t
  :custom
  (envrc-debug t)
  (envrc-none-lighter nil)
  (envrc-show-summary-in-minibuffer nil)
  :init
  (defalias 'direnv-reload #'envrc-reload)
  (defalias 'direnv-allow #'envrc-allow)
  :hook ((prog-mode text-mode conf-mode comint-mode) . envrc-mode))

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
  :init
  (magit-auto-revert-mode t))

(let ((linux-font '(
                    :family "Iosevka"
                    :height 105
                    )))
  (eval `(use-package faces
           :ensure nil
           :custom-face
           (default ((((type x pgtk)) ,linux-font)))
           (fixed-pitch ((((type x pgtk)) ,linux-font))))))

(use-package cua-base
  :custom (cua-mode t))

(use-package debbugs
  :ensure t
  :custom
  ((debbugs-gnu-use-threads nil)
   (debbugs-servers
    '(("debian.org"
       :wsdl "https://bugs.debian.org/cgi-bin/soap.cgi"
       :bugreport-url "https://bugs.debian.org/cgi-bin/bugreport.cgi")))
   (debbugs-port "debian.org")
   (debbugs-gnu-default-packages '("xdg-desktop-portal" "src:xdg-desktop-portal"))
   (debbugs-gnu-default-severities debbugs-gnu-all-severities)
   (debbugs-gnu-suppress-closed nil)
   (debbugs-gnu-mail-backend 'rmail)))

(require 'server)
(unless (server-running-p)
  (server-start))
