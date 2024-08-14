;; -*- lexical-binding: t; -*-
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

(defun backward-kill-line ()
  (interactive)
  (kill-line 0))

(defun open-previous-line ()
  (interactive)
  (move-beginning-of-line 1)
  (open-line 1)
  (indent-for-tab-command))

(use-package emacs
  :custom
  (auto-save-file-name-transforms
   `((".*" ,(locate-user-emacs-file "backups/") t)))
  (blink-cursor-mode nil)
  (column-number-mode t)
  (create-lockfiles nil)
  (deactivate-mark nil)
  (delete-selection-mode t)
  (enable-recursive-minibuffers t)
  (frame-title-format "%b")
  (indent-tabs-mode nil)
  (indicate-empty-lines t)
  (inhibit-startup-screen t)
  (kill-whole-line t)
  (make-backup-files nil)
  (native-comp-async-report-warnings-errors nil)
  (read-hide-char ?\u2022) ;; BULLET
  (ring-bell-function #'ignore)
  (safe-local-variable-directories (list
                                    (expand-file-name "~/Developer/grecal")))
  (tab-always-indent 'complete)
  (use-dialog-box nil)
  (use-short-answers t)
  (warning-minimum-level :error)
  :config
  (dolist (key '("M-u" "s-q" "M-s"))
    (unbind-key key global-map))
  :bind
  ("C-x 2" . split-and-follow-vertically)
  ("C-x 3" . split-and-follow-horizontally)
  ("C-k" . kill-whole-line)
  ("C-w" . c-w-dwim)
  ("C-x x" . revert-buffer-quick)
  ("C-z" . undo)
  ("C-S-z" . undo-redo)
  ("s-Z" . undo-redo)
  ("M-i" . completion-at-point)
  ("M-k" . kill-sexp)
  ("M-;" . comment-line)
  ("M-," . pop-global-mark)
  ("M-SPC" . just-one-space)
  (:map minibuffer-local-map
        ("C-u" . backward-kill-sentence)))

(use-package repeat
  :init (repeat-mode))

(use-package isearch
  :bind
  ([remap isearch-forward] . isearch-forward-regexp)
  (:map isearch-mode-map
        ("ESC" . isearch-exit)))

(use-package paren
  :custom
  (show-paren-mode nil)
  (show-paren-context-when-offscreen 'overlay))

(use-package display-fill-column-indicator
  :custom
  (display-fill-column-indicator-character ?\u2595) ;; RIGHT ONE EIGHT BLOCK
  (display-fill-column-indicator-column 80))

(use-package display-line-numbers
  :custom
  (display-line-numbers-grow-only t)
  (display-line-numbers-width-start 1000))

(use-package eldoc
  :custom
  (eldoc-minor-mode-string nil))

(use-package tab-line
  :config
  (define-advice tab-line-select-tab-buffer (:around (fun &rest args) dedicated)
    (let ((dedicated (window-dedicated-p)))
      (apply fun args)
      (set-window-dedicated-p (selected-window) dedicated)))
  :custom
  (global-tab-line-mode t)
  (tab-line-new-button-show nil)
  (tab-line-close-button-show nil)
  (tab-line-switch-cycling nil))

(use-package xt-mouse
  :hook (tty-setup . xterm-mouse-mode))

(use-package mwheel
  :custom
  (mouse-autoselect-window t)
  (mouse-wheel-progressive-speed nil)
  (mouse-wheel-scroll-amount '(1 ((shift) . 1)))
  (mouse-wheel-scroll-amount-horizontal 1)
  (mouse-wheel-tilt-scroll t)
  (scroll-conservatively 101)
  (scroll-step 1))

(use-package compile
  :custom
  (compilation-always-kill t)
  (compilation-ask-about-save nil)
  (compilation-scroll-output 'first-error)
  (compile-command "make "))

(use-package shell
  :functions lina-shell-mode-hook
  :custom
  (async-shell-command-buffer 'new-buffer)
  (explicit-shell-file-name "/bin/zsh")
  (explicit-zsh-args '("--interactive"))
  (shell-command-prompt-show-cwd t)
  (shell-kill-buffer-on-exit t)
  :config
  (defun my-shell-hook ()
    (setq-local comint-process-echoes t))
  :hook (shell-mode . my-shell-hook))

(use-package autorevert
  :init (auto-revert-mode t)
  :custom (auto-revert-mode-text ""))

(use-package help
  :custom
  (help-window-select t)
  :bind
  ("C-h c" . describe-char))

(use-package man
  :custom (Man-notify-method 'aggressive))

(use-package cus-edit
  :bind
  ("C-h g" . customize-group)
  ("C-h ," . customize-variable))

(use-package face-remap
  :init
  (defun turn-off-text-scale ()
    (interactive)
    (text-scale-mode -1)))

(use-package info
  :bind ("C-h s" . info-lookup-symbol))

(use-package finder
  :config
  (define-advice finder-commentary (:around (fun &rest args) no-shrink)
    (cl-letf (((symbol-function 'shrink-window-if-larger-than-buffer) #'ignore))
      (apply fun args))))

;; tls
;; gnutls-min-prime-bits 3072
;; network-security-level 'medium
(provide 'lina-core)
