;; -*- lexical-binding: t; -*-
(add-to-list 'load-path (locate-user-emacs-file "lisp/"))
(require 'use-package)
(require 'bind-key)
(autoload 'elisp-enable-lexical-binding "elisp-mode" nil t)
(autoload 'text-scale-mode "face-remap" nil t)

(load (setq custom-file (locate-user-emacs-file "custom.el")) t t t t)

;;(use-package exec-path-from-shell
;;  :custom
;;  (exec-path-from-shell-variables
;;   '("PATH" "MANPATH" "INFOPATH" "GOPATH" "JAVA_HOME"))
;;  :hook (after-init . exec-path-from-shell-initialize))

(defun backward-kill-line ()
  (interactive)
  (kill-line 0))

(defun open-previous-line ()
  (interactive)
  (move-beginning-of-line 1)
  (open-line 1)
  (indent-for-tab-command))

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

(defun turn-off-text-scale ()
  (interactive)
  (text-scale-mode -1))

(setopt
 ange-ftp-default-user "anonymous"
 ange-ftp-generate-anonymous-password "guest"
 ange-ftp-try-passive-mode t
 async-shell-command-buffer 'new-buffer
 auto-insert-directory (locate-user-emacs-file "auto-insert/")
 auto-insert-alist `((,(rx "." (or "tex" "ltx") string-end) . "latex")
                     ("\\.el\\'" . ,(lambda ()
                                      (elisp-enable-lexical-binding))))
 auto-revert-mode t
 auto-revert-mode-text ""
 blink-cursor-mode nil
 bookmark-save-flag 1
 bookmark-fringe-mark nil
 column-number-mode t
 compilation-always-kill t
 compilation-ask-about-save nil
 compilation-scroll-output 'first-error
 compile-command "make "
 create-lockfiles nil
 css-indent-offset 2
 deactivate-mark nil
 delete-by-moving-to-trash t
 delete-selection-mode t
 display-buffer-alist
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
                       "elfeed-entry"))
      display-buffer-below-selected)
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
                                "TeX Help"))
               ,(rx (or "shell") "*" eos)))
      display-buffer-in-side-window
      (window-height . ,(/ 1.0 3)))))
 display-buffer-base-action '((display-buffer-reuse-window
                               display-buffer-in-previous-window
                               display-buffer-reuse-mode-window
                               display-buffer-use-least-recent-window))
 display-fill-column-indicator-character ?\u2595 ;; RIGHT ONE EIGHT BLOCK
 display-fill-column-indicator-column 80
 display-line-numbers-grow-only t
 display-line-numbers-width-start 1000
 eldoc-echo-area-use-multiline-p nil
 eldoc-minor-mode-string nil
 enable-recursive-minibuffers t
 frame-title-format "%b"
 indent-tabs-mode nil
 indicate-empty-lines t
 inhibit-startup-screen t
 ispell-program-name "hunspell"
 ispell-dictionary "en_GB"
 kill-whole-line t
 make-backup-files nil
 Man-notify-method 'aggressive
 mouse-autoselect-window t
 mouse-wheel-progressive-speed nil
 mouse-wheel-scroll-amount '(1 ((shift) . 1))
 mouse-wheel-scroll-amount-horizontal 1
 mouse-wheel-tilt-scroll t
 native-comp-async-report-warnings-errors nil
 package-archives nil
 ;; '(("gnu" . "https://elpa.gnu.org/packages/")
 ;;   ("nongnu" . "https://elpa.nongnu.org/nongnu/")
 ;;   ("melpa" . "https://melpa.org/packages/")
 ;;   ("gnu-devel" . "https://elpa.gnu.org/devel/"))
 package-archive-priorities '(("gnu-devel" . -1))
 read-hide-char ?\u2022 ;; BULLET
 recentf-max-menu-items most-positive-fixnum
 recentf-max-saved-items 80
 recentf-mode t
 repeat-mode t
 ring-bell-function #'ignore
 safe-local-variable-directories
 (list
  (expand-file-name "~/Developer/grecal"))
 save-place-mode t
 savehist-mode t
 scroll-conservatively 101
 scroll-step 1
 sh-basic-offset 2
 shell-command-prompt-show-cwd t
 shell-kill-buffer-on-exit t
 show-paren-mode nil
 show-paren-context-when-offscreen 'overlay
 switch-to-buffer-obey-display-actions t
 switch-to-buffer-in-dedicated-window 'pop
 tab-always-indent 'complete
 use-dialog-box nil
 use-package-always-defer t
 use-short-answers t
 warning-minimum-level :error
 vc-handled-backends '(Git)
 xref-prompt-for-identifier nil
 xref-search-program (if (executable-find "rg") 'ripgrep 'grep))

(require 'my-comint)
(require 'my-dired)
(require 'my-eglot)
(require 'my-elisp-mode)
(require 'my-help)
(require 'my-icomplete)
(require 'my-ielm)
(require 'my-keys)
(require 'my-makefile-mode)
(require 'my-project)
(require 'my-tab-line)
(require 'my-theme)
(require 'my-org)

(add-hook 'tty-setup-hook #'xterm-mouse-mode)
(add-hook 'prog-mode-hook (defun my-prog-mode-hook ()
                            (setq-local show-trailing-whitespace t
                                        truncate-lines t)
                            (display-line-numbers-mode)
                            (display-fill-column-indicator-mode)
                            (show-paren-local-mode)))
(add-hook 'asm-mode-hook
          (defun my-asm-mode-hook ()
            (setq-local tab-width 2)
            (when (boundp 'asm-comment-char)
              (local-unset-key (vector asm-comment-char)))
            (unbind-key ":" 'asm-mode-map)))
(add-hook 'sh-base-mode-hook #'flymake-mode)
(add-hook 'yaml-ts-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'visual-line-mode)
(add-hook 'buffer-face-mode-hook
          (defun my-variable-pitch-mode-hook ()
            (setq-local cursor-type
                        (if (bound-and-true-p buffer-face-mode)
                            'bar
                          t))))

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
  ("C-x p f" . consult-find)
  ("C-x p g" . consult-ripgrep)
  ("C-h i" . consult-info))

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
;; (:map embark-symbol-map
;;       ("h" . helpful-symbol))

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
  :ensure t
  :init
  (setopt elfeed-feeds (my-read-from-file
                        (locate-user-emacs-file "elfeed-feeds.eld"))
          elfeed-search-filter "@1-day-ago +unread ")
  :bind (:map elfeed-show-mode-map
              ("q" . quit-window)))

(use-package envrc
  :ensure t
  :init (envrc-global-mode))

;; (reformatter-define prettier
;;   :program "prettier"
;;   :lighter "Prettier"
;;   :args (list "--stdin-filepath" buffer-file-name))

;;; (load "lina-java")
;; (require 'lina-org)
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
          treesit-auto-langs '(java bash yaml json typescript tsx rust))
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



;; (defvar-local hide-cursor--original nil)

;; (define-minor-mode hide-cursor-mode
;;   "https://karthinks.com/software/more-less-emacs/"
;;   :global nil
;;   :lighter " HideCursor"
;;   (if hide-cursor-mode
;;       (setq-local hide-cursor--original cursor-type
;;                   cursor-type nil)
;;     (setq-local cursor-type (or hide-cursor--original t))))

;; (require 'server)
;; (unless (server-running-p)
;;   (server-start))

(add-hook 'after-init-hook
          (lambda ()
            (let ((backups-directory (locate-user-emacs-file "backups/")))
              (unless (file-directory-p backups-directory)
                (make-directory backups-directory))
              (setopt auto-save-file-name-transforms
                      `((".*" ,backups-directory t))))))

(with-eval-after-load 'man
  (advice-add 'Man-completion-table :override #'ignore))
