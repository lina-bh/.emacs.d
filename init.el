;; -*- lexical-binding: t; -*-
(add-to-list 'load-path (locate-user-emacs-file "lisp/"))
(require 'use-package)
(require 'bind-key)

(setq-default package-archives nil
              ;; '(("gnu" . "https://elpa.gnu.org/packages/")
              ;;   ("nongnu" . "https://elpa.nongnu.org/nongnu/")
              ;;   ("melpa" . "https://melpa.org/packages/")
              ;;   ("gnu-devel" . "https://elpa.gnu.org/devel/"))
              package-archive-priorities '(("gnu-devel" . -1))
              use-package-always-defer t)

(load (setq custom-file (locate-user-emacs-file "custom.el"))
      'noerror 'nomessage 'nosuffix 'mustsuffix)

(use-package emacs
  :ensure nil
  :init
  (setopt create-lockfiles nil
          delete-by-moving-to-trash t
          enable-recursive-minibuffers t
          frame-title-format "%b"
          indicate-empty-lines t
          mouse-autoselect-window t
          read-hide-char ?\u2022 ;; BULLET
          ring-bell-function #'ignore
          scroll-conservatively 101
          scroll-step 1
          use-dialog-box nil
          use-short-answers t)
  (defun split-and-follow-vertically ()
    (interactive)
    (let ((window (split-window-below)))
      (select-window window)))
  (defun split-and-follow-horizontally ()
    (interactive)
    (let ((window (split-window-right)))
      (select-window window)))
  (defun c-w-dwim ()
    (interactive)
    (if (use-region-p)
        (kill-region (region-beginning)
                     (region-end))
      (backward-kill-word 1)))
  (dolist (key '("M-u" "s-q" "M-s"))
    (unbind-key key global-map))
  :bind (("C-x 2" . split-and-follow-vertically)
         ("C-x 3" . split-and-follow-horizontally)
         ("C-k" . kill-whole-line)
         ("C-w" . c-w-dwim))
  :bind (:map minibuffer-local-map
              ("C-u" . backward-kill-sentence)))
(use-package warnings
  :ensure nil
  :init (setopt warning-minimum-level :error))
(use-package simple
  :ensure nil
  :init (setopt async-shell-command-buffer 'new-buffer
                column-number-mode t
                deactivate-mark nil
                indent-tabs-mode nil
                kill-whole-line t
                shell-command-prompt-show-cwd t)
  :bind (("C-z" . undo)
         ("C-S-z" . undo-redo)
         ("s-Z" . undo-redo)
         ("C-S-c" . kill-ring-save)
         ("C-S-v" . yank)
         ("M-," . pop-global-mark)
         ("M-SPC" . just-one-space)))
(use-package comp-run
  :ensure nil
  :init (setopt native-comp-async-report-warnings-errors nil))
(use-package files
  :ensure nil
  :init
  (setopt make-backup-files nil
          safe-local-variable-directories
          (list
           (expand-file-name "~/Developer/grecal")))
  (let ((backups-directory (locate-user-emacs-file "backups/")))
    (unless (file-directory-p backups-directory)
      (make-directory backups-directory))
    (setopt auto-save-file-name-transforms
            `((".*" ,backups-directory t))))
  :bind (("C-x x" . revert-buffer-quick)))
(use-package startup
  :ensure nil
  :init (setopt inhibit-startup-screen t))
(use-package mwheel
  :ensure nil
  :init (setopt mouse-wheel-progressive-speed nil
                mouse-wheel-scroll-amount '(1 ((shift) . 1))
                mouse-wheel-scroll-amount-horizontal 1
                mouse-wheel-tilt-scroll t
                mouse-wheel-flip-direction t))
(use-package window
  :ensure nil
  :init
  (setopt display-buffer-base-action '((display-buffer-reuse-window
                                        display-buffer-in-previous-window
                                        display-buffer-reuse-mode-window
                                        display-buffer-use-least-recent-window))
          switch-to-buffer-obey-display-actions t
          switch-to-buffer-in-dedicated-window 'pop)
  (setopt display-buffer-alist
          (cl-labels ((derived-mode (mode)
                        (when (symbolp mode)
                          (setq mode (symbol-name mode)))
                        (cons 'derived-mode (intern (format "%s-mode" mode))))
                      (derived-modes (&rest modes)
                        (let (xs)
                          (dolist (mode modes)
                            (push (derived-mode mode) xs))
                          xs)))
            `((,(rx bos "*" (or "Pp"
                                "elfeed-entry"
                                "Man"))
               (display-buffer-reuse-mode-window
                display-buffer-below-selected))
              ((and (not (or ,@(derived-modes 'Info 'package-menu)
                             "COMMIT_EDITMSG"))
                    (or ,@(derived-modes 'comint
                                         'eshell
                                         'special
                                         'term
                                         'flymake-project-diagnostics
                                         'flymake-diagnostics-buffer
                                         'apropos
                                         'compilation)
                        (category . comint)
                        ,(rx bos "*" (or "Finder"
                                         "Embark"
                                         "TeX Help"
                                         "vterm"
                                         "Agenda Commands"))
                        ,(rx (or "shell") "*" eos)))
               display-buffer-in-side-window
               (window-height . ,(/ 1.0 3))))))
  :bind (("C-x =" . balance-windows)
         ("C-x q" . quit-window)
         ("s-w" . quit-window)))

(use-package repeat
  :ensure nil
  :init (setopt repeat-mode t))
(use-package lisp
  :ensure nil
  :bind ("M-k" . kill-sexp))
(use-package newcomment
  :ensure nil
  :bind ("M-;" . comment-line))
(use-package minibuffer
  :bind ("M-i" . completion-at-point))
(use-package autorevert
  :ensure nil
  :init
  (setopt auto-revert-mode t
          auto-revert-mode-text ""))
(use-package frame
  :ensure nil
  :init (setopt blink-cursor-mode nil))
(use-package delsel
  :ensure nil
  :init (setopt delete-selection-mode t))
(use-package isearch
  :ensure nil
  :bind (:map isearch-mode-map
              ("ESC" . isearch-exit)))
(use-package indent
  :ensure nil
  :init
  (setopt tab-always-indent 'complete)
  (defun indent-region-or-buffer (beg end)
    "Call `indent-region' on either the selected region or the whole buffer."
    (interactive "r")
    (unless (use-region-p)
      (setq beg (point-min)
            end (point-max)))
    (save-excursion
      (indent-region beg end)
      (whitespace-cleanup-region beg end)))
  :bind ("C-c f" . indent-region-or-buffer))

(use-package info
  :ensure nil
  :bind ("C-h s" . info-lookup-symbol))
(use-package help
  :ensure nil
  :init (setopt help-window-select t))
(use-package man
  :ensure nil
  :init (setopt Man-notify-method 'aggressive)
  :config (advice-add 'Man-completion-table :override #'ignore))

(use-package descr-text
  :ensure nil
  :bind ("C-h c" . describe-char))
(use-package cus-edit
  :ensure nil
  :bind (("C-h g" . customize-group)
         ("C-h ," . customize-variable)))

(use-package elisp-mode
  :ensure nil
  :functions elisp-enable-lexical-binding
  :init
  (defun my-elisp-mode-hook ()
    (require 'autoinsert)
    (require 'seq)
    (add-hook 'after-save-hook #'check-parens nil 'local)
    (remove-hook 'flymake-diagnostic-functions #'elisp-flymake-checkdoc t)
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
  :hook (emacs-lisp-mode . flymake-mode)
  :bind (:map emacs-lisp-mode-map
              (";" . comment-dwim)
              ("C-c C-p" . pp-macroexpand-last-sexp))
  :mode ("\\.dir-locals\\(?:-2\\)?\\.el\\'" . emacs-lisp-mode))

(use-package autoinsert
  :ensure nil
  :init
  (setopt auto-insert-directory (locate-user-emacs-file "auto-insert/")
          auto-insert-alist `((,(rx "." (or "tex" "ltx") string-end) . "latex")
                              ("\\.el\\'" . ,(lambda ()
                                               (elisp-enable-lexical-binding))))))
(use-package compile
  :ensure nil
  :init
  (setopt compilation-always-kill t
          compilation-ask-about-save nil
          compilation-scroll-output 'first-error
          compile-command "make "))

(use-package bookmark
  :ensure nil
  :init (setopt bookmark-save-flag 1
                bookmark-fringe-mark nil))
(use-package xref
  :ensure nil
  :init (setopt xref-prompt-for-identifier nil
                xref-search-program (if (executable-find "rg") 'ripgrep 'grep))
  :bind ("M-/" . xref-find-definitions))
(use-package savehist
  :ensure nil
  :init (setopt savehist-mode t))
(use-package saveplace
  :ensure nil
  :init (setopt save-place-mode t))
(use-package recentf
  :ensure nil
  :init (setopt recentf-max-menu-items most-positive-fixnum
                recentf-max-saved-items 80
                recentf-mode t)
  :bind ("C-x C-r" . recentf-open))

(use-package eldoc
  :ensure nil
  :init (setopt eldoc-echo-area-use-multiline-p nil
                eldoc-minor-mode-string nil))
(use-package display-fill-column-indicator
  :ensure nil
  :init (setopt display-fill-column-indicator-character ?\u2595 ;; RIGHT ONE EIGHT BLOCK
                display-fill-column-indicator-column 80))
(use-package display-line-numbers
  :ensure nil
  :init (setopt display-line-numbers-grow-only t
                display-line-numbers-width-start 1000))

(use-package ispell
  :ensure nil
  :init (setopt ispell-program-name "hunspell"
                ispell-dictionary "en_GB"))
(use-package flyspell
  :config
  (unbind-key [down-mouse-2] 'flyspell-mode-map)
  (unbind-key "C-M-i" 'flyspell-mode-map)
  :bind (:map flyspell-mode-map
              ([mouse-3] . flyspell-correct-word)))

(use-package css-mode
  :ensure nil
  :init (setopt css-indent-offset 2))







(use-package paren
  :ensure nil
  :init (setopt show-paren-mode nil
                show-paren-context-when-offscreen 'overlay))



(use-package vc
  :ensure nil
  :init (setopt vc-handled-backends '(Git)))



(use-package shell
  :ensure nil
  :init (setopt shell-kill-buffer-on-exit t))

(use-package comint
  :ensure nil
  :init
  (setopt comint-prompt-read-only t
          comint-scroll-to-bottom-on-input 'this)
  (defun my-shell-mode-hook ()
    (setq-local comint-process-echoes t))
  :hook (shell-mode . my-shell-mode-hook)
  :bind (:map comint-mode-map
              ("<up>" . comint-previous-input)
              ("<down>" . comint-next-input)))

(use-package dired
  :ensure nil
  :init (setopt dired-recursive-deletes 'always
                dired-clean-confirm-killing-deleted-buffers nil
                dired-listing-switches "-aFlh")
  :hook (dired-mode . dired-hide-details-mode)
  :bind (("C-x d" . dired-jump)
         ("C-x C-d" . dired))
  :bind (:map 'dired-mode-map
              ("<mouse-2>" . dired-mouse-find-file)))

(use-package eglot
  :ensure nil
  :init
  (setopt eglot-ignored-server-capabilities '(:inlayHintProvider)
          eglot-report-progress nil
          eglot-events-buffer-config '(:size nil :format full))
  (defun my-eglot-hook ()
    (setq-local eldoc-echo-area-use-multiline-p
                (eglot-managed-p)))
  :config
  (setf (alist-get 'web-mode eglot-server-programs)
        (alist-get 'html-mode eglot-server-programs))
  :hook (eglot-managed-mode . my-eglot-hook)
  :bind (:map eglot-mode-map
              ("<f2>" . eglot-rename)))

(use-package pp
  :ensure nil
  :init (define-advice pp-display-expression
            (:after (&rest args) readonly)
          (let ((out-buffer-name (cadr args)))
            (with-current-buffer out-buffer-name
              (view-mode)
              (flymake-mode -1))))
  :bind ("M-:" . pp-eval-expression))



(use-package finder
  :ensure nil
  :config
  (define-advice finder-commentary
      (:around (fun &rest args) no-shrink)
    (cl-letf (((symbol-function 'shrink-window-if-larger-than-buffer) #'ignore))
      (apply fun args))))

(use-package icomplete
  :ensure nil
  :init
  (setopt fido-vertical-mode t
          icomplete-matches-format ""
          icomplete-show-matches-on-no-input t
          icomplete-compute-delay 0)
  (defun my-icomplete-hook ()
    (setq-local
     truncate-lines t
     completion-auto-help nil
     completion-styles (default-value 'completion-styles)))
  :hook (icomplete-minibuffer-setup . my-icomplete-hook)
  :bind (:map icomplete-fido-mode-map
              ("TAB" . icomplete-fido-ret)
              ("<tab>" . icomplete-fido-ret)))

(use-package ielm
  :ensure nil
  :init
  (defun ielm-C-c ()
    (interactive)
    (comint-skip-input)
    (ielm-return))
  :bind (:map ielm-map
              ("C-c C-c" . ielm-C-c)))

(use-package org
  :ensure nil
  :init
  (setopt org-adapt-indentation nil
          org-link-descriptive nil
          org-export-backends '(html latex)
          org-babel-load-languages '((emacs-lisp . t)
                                     (python . t)
                                     (R . t)
                                     (shell . t)
                                     ;; (latex . t)
                                     )
          org-babel-python-mode "python3"
          org-confirm-babel-evaluate nil
          org-refile-targets '((nil :maxlevel . 2))
          org-html-postamble nil
          org-startup-folded 'show2levels
          org-format-latex-options
          (list :foreground 'default
                :background "Transparent"
                :scale 2.0
                :html-foreground "Black"
                :html-background "Transparent"
                :html-scale 1.0
                :matchers '("begin" "$1" "$" "$$" "\\(" "\\["))
          org-src-lang-modes `(,@(and (fboundp #'LaTeX-mode)
                                      (mapcar (lambda (lang)
                                                (cons lang 'LaTeX))
                                              '("latex" "beamer")))
                               ,@(and (treesit-language-available-p 'bash)
                                      (mapcar (lambda (lang)
                                                (cons lang 'bash-ts))
                                              '("sh" "shell" "bash"))))
          org-src-preserve-indentation t
          org-src-window-setup 'plain
          org-latex-classes
          '(("article" "\\documentclass[a4paper,11pt]{article}"
             ("\\section{%s}" . "\\section*{%s}")
             ("\\subsection{%s}" . "\\subsection*{%s}")
             ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
             ("\\paragraph{%s}" . "\\paragraph*{%s}")
             ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
          org-latex-default-packages-alist
          '(("AUTO" "inputenc" t ("pdflatex"))
            ("T1" "fontenc" t ("pdflatex"))
            ("" "graphicx" t)
            ("" "amsmath" t)
            ("bookmarks=false,colorlinks=true,urlcolor=blue,linkcolor=,citecolor="
             "hyperref" nil))
          org-latex-packages-alist '(("margin=1in" "geometry")
                                     ("" "lmodern")
                                     ("" "minted")
                                     ("british" "babel"))
          org-latex-src-block-backend 'minted
          org-latex-minted-options '(("breaklines" . t)
                                     ("ignorelexererrors" . t)
                                     ("samepage" . t))
          org-latex-remove-logfiles nil
          org-latex-pdf-process
          (nreverse
           '("%latex -interaction=nonstopmode -output-directory=%o -shell-escape \
-draftmode %f"
             "%latex -interaction=batchmode -halt-on-error -shell-escape \
-output-directory=%o %f"))
          org-export-with-smart-quotes t
          org-agenda-window-setup 'current-window)
  (defun my-org-hook ()
    (setq-local line-spacing 0.2)
    (variable-pitch-mode)
    (flyspell-mode)
    (face-remap-add-relative 'variable-pitch
                             :family "Liberation Serif"
                             :height 120))
  :hook ((org-mode org-agenda-mode) . my-org-hook)
  :hook (org-babel-after-execute . org-redisplay-inline-images)
  :bind ("C-c C-l" . org-store-link)
  :bind (:map org-mode-map
              ("C-c l" . org-latex-preview)
              ("C-c p" . org-latex-export-to-pdf)
              ("C-c t" . org-babel-tangle)
              ("C-c s p" . org-latex-export-section-to-pdf)
              ("C-t" . org-todo))
  :bind (:map org-src-mode-map
              ("C-c C-c" . org-edit-src-exit)))


;; (defvar org-preview-latex-default-process (if (executable-find "dvisvgm")
;;                                               'dvisvgm
;;                                             'dvipng))

(use-package project
  :ensure nil
  :init
  (defun my-project-prompt-dir ()
    (let* (history-add-new-input
           (dir-choice "... (choose a dir)")
           (known-roots (project-known-project-roots))
           (pr-dir (completing-read "Select project: "
                                    (append
                                     (list default-directory)
                                     known-roots
                                     (list dir-choice))
                                    nil t nil 'known-roots)))
      (cond
       ((string-empty-p pr-dir) default-directory)
       ((string-equal pr-dir dir-choice)
        (read-directory-name "Select directory: " default-directory nil t))
       (t
        pr-dir))))
  (setopt project-prompter #'my-project-prompt-dir
          project-vc-extra-root-markers '(".project" "pom.xml" "Cargo.toml"))
  (defun project-add-dir-local-variable ()
    (interactive)
    (let ((default-directory (project-root (project-current))))
      (call-interactively #'add-dir-local-variable)))
  :bind (("M-!" . project-shell-command)
         ("M-&" . project-async-shell-command)))

(use-package make-mode
  :ensure nil
  :init
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
  (defun my-make-hook ()
    (setq-local indent-line-function #'indent-makefile
                tab-always-indent t
                whitespace-style '(face indentation tab-mark))
    (indent-tabs-mode)
    (whitespace-mode))
  :hook (makefile-mode . my-make-hook))

(use-package tab-line
  :ensure nil
  :init (setopt global-tab-line-mode t
                tab-line-new-button-show nil
                tab-line-close-button-show nil
                tab-line-switch-cycling nil)
  :config (with-eval-after-load 'tab-line
            (define-advice tab-line-select-tab-buffer
                (:around (fun &rest args) dedicated)
              (let ((dedicated (window-dedicated-p)))
                (apply fun args)
                (set-window-dedicated-p (selected-window) dedicated)))))

(use-package modus-themes
  :ensure nil
  :load-path (lambda ()
               (expand-file-name "./themes" data-directory))
  :init
  (setopt modus-themes-variable-pitch-ui t
          modus-themes-mixed-fonts t
          modus-themes-prompts '(intense)
          modus-themes-to-toggle '(modus-operandi modus-vivendi)
          modus-themes-common-palette-overrides
          '((fringe unspecified)
            (bg-line-number-inactive unspecified)
            (bg-line-number-active unspecified)
            (fg-line-number-active fg-main)))
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

(use-package ange-ftp
  :ensure nil
  :init (setopt
         ange-ftp-default-user "anonymous"
         ange-ftp-generate-anonymous-password "guest"
         ange-ftp-try-passive-mode t))

(use-package tramp-rclone
  :ensure nil
  :config
  (let* ((meth (alist-get tramp-rclone-method
                          tramp-methods nil nil #'string=))
         (mount-args (alist-get 'tramp-mount-args meth)))
    (setf mount-args '(("--no-unicode-normalization"
                        "--dir-cache-time" "0s"
                        "--vfs-cache-mode" "full")))
    mount-args))

(use-package xt-mouse
  :ensure nil
  :hook (tty-setup . xterm-mouse-mode))

(use-package prog-mode
  :ensure nil
  :init (defun my-prog-mode-hook ()
          (setq-local show-trailing-whitespace t
                      truncate-lines t)
          (display-line-numbers-mode)
          (display-fill-column-indicator-mode)
          (show-paren-local-mode))
  :hook (prog-mode . my-prog-mode-hook))

(use-package asm-mode
  :ensure nil
  :init (defun my-asm-mode-hook ()
          (setq-local tab-width 2)
          (when (boundp 'asm-comment-char)
            (local-unset-key (vector asm-comment-char)))
          (unbind-key ":" 'asm-mode-map))
  :hook (asm-mode . my-asm-mode-hook))

(use-package sh-script
  :ensure nil
  :init (setopt sh-basic-offset 2)
  :hook (sh-base-mode . flymake-mode))

(use-package text-mode
  :ensure nil
  :hook (text-mode . visual-line-mode))

(use-package face-remap
  :ensure nil
  :commands text-scale-mode
  :init
  (defun turn-off-text-scale ()
    (interactive)
    (text-scale-mode -1))
  (defun my-variable-pitch-mode-hook ()
    (setq-local cursor-type
                (if (bound-and-true-p buffer-face-mode)
                    'bar
                  t)))
  :hook (buffer-face-mode . my-variable-pitch-mode-hook))

(use-package orderless
  :ensure t
  :demand t
  :config
  (setopt orderless-component-separator " +\\|[-/]"
          completion-styles '(orderless basic)))

(use-package marginalia
  :ensure t
  :init (setopt marginalia-mode t))

(use-package consult
  :ensure t
  :init
  (setopt consult-async-split-style nil
          consult-find-args "find ."
          consult-line-start-from-top t
          consult-preview-allowed-hooks '()
          completion-in-region-function #'consult-completion-in-region
          xref-show-definitions-function #'consult-xref
          xref-show-xrefs-function #'consult-xref)
  :bind
  ("M-g" . consult-imenu)
  ("C-x C-g" . consult-imenu)
  ("C-x p f" . consult-find)
  ("C-x p g" . consult-ripgrep)
  ("C-h i" . consult-info)
  ("C-c a" . consult-org-agenda))

(use-package embark-consult
  :ensure t)

(use-package embark
  :ensure t
  :init
  (setopt embark-indicators '(embark-minimal-indicator
                              embark-highlight-indicator
                              embark-isearch-highlight-indicator)
          embark-cycle-key "<tab>"
          prefix-help-command #'embark-prefix-help-command)
  (unbind-key "C-h" 'help-map)
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
        ("l" . eglot-code-actions)))

(use-package corfu
  :ensure t
  :init
  (setopt corfu-quit-no-match nil
          global-corfu-mode t))

(use-package cape
  :ensure t
  :config
  (defun my-elisp-cape-hook ()
    (add-hook 'completion-at-point-functions
              #'cape-elisp-symbol -1 t))
  :hook ((emacs-lisp-mode inferior-emacs-lisp-mode)
         .
         my-elisp-cape-hook))

(use-package magit
  :ensure t
  :commands magit-dotfiles
  :init
  (setopt magit-display-buffer-function #'display-buffer
          magit-commit-show-diff nil)
  :config
  (setopt magit-auto-revert-mode t)
  (defun magit-dotfiles ()
    "Magit on dotfiles repo for the duration of a recursive edit."
    (interactive)
    (let ((magit-git-global-arguments
           `(,(substitute-env-vars "--git-dir=$HOME/.dotfiles")
             ,(substitute-env-vars "--work-tree=$HOME")
             ,@magit-git-global-arguments)))
      (magit-status "~")
      (recursive-edit))))

(use-package puni
  :ensure t
  :config
  (defvar-keymap my-puni-repeat-map
    :repeat t
    "." #'puni-slurp-forward
    "r" #'puni-raise)
  (put #'puni-slurp-forward 'repeat-map 'my-puni-repeat-map)
  (put #'puni-raise 'repeat-map 'my-puni-repeat-map)
  (defun my-puni-c-w-dwim ()
    (interactive)
    (if (use-region-p)
        (puni-kill-region)
      (backward-kill-sexp)))
  (defun my-puni-kill-whole-line ()
    (interactive)
    (let ((kill-whole-line t))
      (move-beginning-of-line nil)
      (puni-kill-line)))
  :hook
  (puni-mode . electric-pair-local-mode)
  (prog-mode . puni-mode)
  (yaml-mode . puni-mode)
  :bind (:map puni-mode-map
              ("C-c r" . puni-raise)
              ("C-c ." . puni-slurp-forward)
              ("C-c s" . puni-splice)
              ("C-9" . puni-wrap-round)
              ("C-<backspace>" . puni-backward-kill-line)
              ("C-k" . my-puni-kill-whole-line)
              ("C-w" . my-puni-c-w-dwim)))

(use-package aggressive-indent
  :ensure t
  :hook (emacs-lisp-mode . aggressive-indent-mode))

(use-package nix-mode
  :ensure t)

(use-package reformatter
  :ensure t
  :init
  (reformatter-define nixfmt
    :program "nixfmt"
    :lighter " Fmt")
  :config
  (define-advice reformatter--do-region (:around (fun &rest args)
                                                 quit-not-delete)
    (cl-letf (((symbol-function #'delete-windows-on)
               #'quit-windows-on))
      (apply fun args)))
  :hook (nix-mode . nixfmt-on-save-mode))

(defun my-read-from-file (path)
  (with-temp-buffer
    (insert-file-contents path)
    (read (current-buffer))))

(use-package elfeed
  :disabled t
  :ensure t
  :init
  (setopt elfeed-feeds (ignore-errors
                         (my-read-from-file
                          (locate-user-emacs-file "elfeed-feeds.eld")))
          elfeed-search-filter "@1-day-ago +unread ")
  :bind (:map elfeed-show-mode-map
              ("q" . quit-window)))

(use-package envrc
  :disabled t
  :ensure t
  :init (envrc-global-mode))

(use-package yaml-mode
  :ensure t
  :hook (yaml-mode . display-line-numbers-mode)
  :hook (yaml-mode . electric-pair-local-mode))

(use-package prettier
  :ensure t
  :hook (yaml-mode . prettier-mode))

(defun major-mode? ()
  "What fucking mode is this?"
  (interactive)
  (message "%s" major-mode))

;;; (load "lina-java")
;;(load "lina-tex")
;;(load "lina-js")
;;(load "lina-python")
;;(when (package-installed-p 'poly-R)
;;  (load "lina-poly"))
;;(pcase system-type
;;  ('windows-nt (load "lina-w32"))
;;  ('darwin (load "lina-macos")))

(use-package treesit-auto
  :if (treesit-available-p)
  :ensure t
  :init
  (setopt global-treesit-auto-mode t
          treesit-auto-langs '(java
                               bash
                               json
                               typescript
                               tsx
                               rust
                               dockerfile
                               lua))
  :config
  (treesit-auto-add-to-auto-mode-alist))

(use-package rust-mode
  :ensure t
  :defines rust-mode-map
  :init
  (setopt rust-mode-treesitter-derive t
          rust-format-on-save t)
  :mode ("\\.rs\\'" . rust-mode)
  :bind (:map rust-mode-map
              ("C-c C-c" . rust-compile)
              ("C-c C-k" . rust-check)
              ("C-c f" . eglot-format)))

(defun delete-visited-file ()
  "Delete the file in the current buffer."
  (interactive)
  (let* ((buffer (current-buffer))
         (file-name (buffer-file-name buffer)))
    (if file-name
        (when (y-or-n-p (format "Delete %s?" file-name))
          (funcall-interactively #'delete-file file-name)
          (kill-buffer buffer))
      (message "Buffer not visiting any file"))))

(defun which ()
  "Show the path to a command."
  (interactive)
  (if-let* ((command (read-shell-command "Which command: "))
            (path (executable-find command)))
      (message "%s" path)
    (message "%s is not recognized as an internal or external command, operable\
 program or batch file." command)))

;; (defun backward-kill-line ()
;;   (interactive)
;;   (kill-line 0))

;; (defun open-previous-line ()
;;   (interactive)
;;   (move-beginning-of-line 1)
;;   (open-line 1)
;;   (indent-for-tab-command))

;; (defvar-local hide-cursor--original nil)

;; (define-minor-mode hide-cursor-mode
;;   "https://karthinks.com/software/more-less-emacs/"
;;   :global nil
;;   :lighter " HideCursor"
;;   (if hide-cursor-mode
;;       (setq-local hide-cursor--original cursor-type
;;                   cursor-type nil)
;;     (setq-local cursor-type (or hide-cursor--original t))))

(require 'server)
(unless (server-running-p)
  (server-start))
