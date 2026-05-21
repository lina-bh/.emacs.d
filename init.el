;; -*- lexical-binding: t; -*-
(autoload 'setq-mode-local "mode-local")

;;; initialisation & general behaviour
(setq-default custom-file (locate-user-emacs-file "custom.el")
	      use-package-always-defer t
	      use-package-enable-imenu-support t
	      package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
				 ("nongnu" . "https://elpa.nongnu.org/nongnu/")
				 ("melpa" . "https://melpa.org/packages/")))
(load custom-file t)

(use-package package
  :ensure nil
  :custom
  (package-install-upgrade-built-in t))

(use-package emacs
  :ensure nil
  :preface
  (defconst linux-font '(:family "Iosevka Nerd Font" :height 105))
  :custom
  (auto-save-default nil)
  (create-lockfiles nil)
  (delete-selection-mode t)
  (eldoc-minor-mode-string nil)
  (help-window-select t)
  (find-function-mode t)
  (inhibit-startup-screen t)
  (initial-scratch-message nil)
  (make-backup-files nil)
  (menu-bar-mode nil)
  (mouse-autoselect-window t)
  (repeat-mode t)
  (savehist-mode t)
  (save-place-mode t)
  (save-interprogram-paste-before-kill t)
  (tool-bar-mode nil)
  (use-short-answers t)
  (view-read-only t)
  (warning-minimum-level :emergency)
  :custom-face
  (default ((((type x pgtk)) ,linux-font)))
  (fixed-pitch ((((type x pgtk)) ,linux-font)))
  :bind
  ("M-u" . ignore)
  ("C-z" . undo)
  ("M-," . pop-to-mark-command)
  ("C-," . pop-global-mark)
  ("M-;" . comment-line)
  (:map ctl-x-map
	("x" . revert-buffer-quick)))

;;; look & feel

(use-package window
  :ensure nil
  :custom
  (display-buffer-base-action '((display-buffer-reuse-window
				 display-buffer-reuse-mode-window
				 display-buffer-use-least-recent-window
				 display-buffer-in-direction)
				(mode . (lisp-interaction-mode))
				(direction . right)))
  (display-buffer-alist
   `(((or
       (category . comint)
       (category . warning)
       (derived-mode . flymake-diagnostics-buffer-mode)
       ,(rx bos "*" (or "Help" "ielm")))
      (display-buffer-in-side-window)
      (window-height . 18)
      (slot . 0))
     ((category . transient)
      display-buffer-at-bottom
      (dedicated . t)
      (inhibit-same-window . t))
     ("*Completions*"
      (display-buffer-reuse-window
       display-buffer-at-bottom))
     ((or
       (derived-mode . dired-mode)
       ,(rx bos "*Customize"))
      (display-buffer-reuse-mode-window))
     (,(rx bos "*Pp")
      (display-buffer-below-selected))
     (t ,@display-buffer-base-action)))
  (switch-to-buffer-in-dedicated-window 'pop)
  (switch-to-buffer-obey-display-actions t))

(use-package tooltip
  :ensure nil
  :custom
  (tooltip-delay 0.1))

;;; minibuffer

(use-package minibuffer
  :ensure nil
  :custom-face
  (completions-annotations ((t :underline nil :inherit (italic shadow))))
  :custom
  (completion-styles '(flex basic))
  (completions-group t)
  :bind
  ("M-i" . completion-at-point)
  (:map minibuffer-local-map
	("C-u" . kill-whole-line)))

(use-package icomplete
  :ensure nil
  :demand t
  :custom
  (fido-vertical-mode t)
  (icomplete-show-matches-on-no-input t)
  (icomplete-matches-format "")
  :bind
  (:map icomplete-minibuffer-map
	("C-." . nil))
  (:map icomplete-fido-mode-map
	("C-." . nil)
	("TAB" . icomplete-fido-ret)))

;;; navigation

(use-package recentf
  :ensure nil
  :custom
  (recentf-mode t)
  (recentf-max-saved-items nil)
  (recentf-exclude (list (rx bos "/nix/store"))))

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
  ("M-s" . isearch-forward-symbol)
  (:map isearch-mode-map
	("ESC" . isearch-exit)))

(use-package imenu
  :ensure nil
  :custom
  (imenu-auto-rescan t)
  (imenu-flatten 'prefix)
  :bind
  ("M-g" . imenu))

;;; integrations

(use-package tramp
  :ensure nil
  :custom
  (tramp-save-ad-hoc-proxies t)
  (tramp-show-ad-hoc-proxies t))

(use-package eshell
  :ensure nil
  :bind
  (:map project-prefix-map
	("s" . eshell)))

(use-package flymake
  :ensure t
  :pin gnu
  :hook
  ((sh-base-mode) . flymake-mode))

;;; built-in major modes

(use-package treesit
  :ensure nil
  :custom
  (treesit-auto-install-grammar 'always)
  (treesit-enabled-modes '(bash-ts-mode markdown-ts-mode))
  (treesit-font-lock-level 4))

(use-package dired
  :ensure nil
  :custom
  (dired-listing-switches "-alZ")
  (dired-kill-when-opening-new-dired-buffer t)
  :bind
  (:map ctl-x-map
	("d" . dired-jump-other-window))
  (:map dired-mode-map
	([remap dired-mouse-find-file-other-window] . dired-mouse-find-file)))

(use-package customize
  :ensure nil
  :bind
  (:map help-map
        ("g" . customize-group-other-window)
	("C-h" . nil)))

(use-package conf-mode
  :ensure nil
  :mode
  ((rx "." (or "container" "volume" "service") eos) . conf-desktop-mode))

(use-package js
  :ensure nil
  :mode ((rx ".conflist" eos) . js-json-mode))

(use-package sh-script
  :ensure nil
  :custom
  (sh-basic-offset 2))

;;; built-in minor modes

(use-package pp
  :ensure nil
  :config
  (define-advice pp-display-expression
      (:after (&rest args) readonly)
    (let ((out-buffer-name (cadr args)))
      (with-current-buffer out-buffer-name
	(view-mode))
      (pop-to-buffer out-buffer-name)))
  :bind
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
  (emacs-lisp-mode . auto-insert))

;;; third-party completion

(use-package cape
  :ensure t
  :init
  (setq-mode-local emacs-lisp-mode
		   completion-at-point-functions '(cape-elisp-symbol t)))

(use-package consult
  :ensure t
  :custom
  (consult-async-split-style nil)
  (completion-in-region-function #'consult-completion-in-region)
  (xref-show-xrefs-function #'consult-xref)
  :bind
  ;; ("M-g" . consult-imenu)
  (:map ctl-x-map
	("C-r" . consult-recent-file))
  (:map project-prefix-map
	("g" . consult-grep)
	("f" . consult-find))
  (:map help-map
	("i" . consult-info)))

(use-package embark
  :ensure t
  :custom
  (embark-indicators '(embark-minimal-indicator
		       embark-highlight-indicator
		       embark-isearch-highlight-indicator))
  (prefix-help-command #'embark-prefix-help-command)
  :bind
  ("C-." . embark-act)
  ("M-." . embark-dwim)
  (:map help-map
	("b" . embark-bindings)))

(use-package embark-consult
  :ensure t)

(use-package marginalia
  :ensure t
  :custom
  (marginalia-mode t))

;;; third-party minor modes

(use-package smartparens
  :ensure t
  :init
  (require 'smartparens-config)
  :custom
  (sp-echo-match-when-invisible nil)
  (sp-escape-quotes-after-insert nil)
  (sp-highlight-pair-overlay nil)
  :config
  (defun lina-sp-mode-hook ()
    (show-paren-local-mode -1)
    (show-smartparens-mode t))
  (defun sp-c-w-dwim (beg end &optional prefix)
    (interactive "Rp")
    (if (and beg end)
	(sp-kill-region beg end)
      (sp-backward-kill-sexp)))
  :hook
  (smartparens-mode . lina-sp-mode-hook)
  (lisp-data-mode . smartparens-strict-mode)
  :bind
  (:map smartparens-mode-map
	("C-w" . sp-c-w-dwim)
	("C-c ." . sp-forward-slurp-sexp)
	("C-c s" . sp-splice-sexp)
	("C-c r" . sp-raise-sexp))
  (:repeat-map smartparens-mode-repeat-map
	       ("." . sp-forward-slurp-sexp)))

(use-package aggressive-indent
  :ensure t
  :hook (lisp-data-mode . aggressive-indent-mode))

;;; third-party integrations

(use-package dumb-jump
  :ensure t
  :init
  (setq-default xref-backend-functions '(dumb-jump-xref-activate))
  (setq-mode-local emacs-lisp-mode
		   xref-backend-functions '(dumb-jump-xref-activate elisp--xref-backend t)))

(use-package gptel
  :ensure t
  :custom
  (gptel-log-level 'debug)
  (gptel-include-reasoning t)
  (gptel-model 'gemini-2.5-flash)
  (gptel-default-mode 'markdown-ts-mode)
  :config
  (setopt gptel-backend (gptel-make-gemini "Gemini"
			  :key (gptel-api-key-from-auth-source "generativelanguage.googleapis.com")
			  :stream t)))

(use-package magit
  :ensure t
  :preface
  (setq magit-define-global-key-bindings nil)
  :custom
  (magit-display-buffer-function #'display-buffer)
  :bind
  (:map ctl-x-map
	("g" . magit-dispatch))
  (:map mode-specific-map
	("g" . magit-file-dispatch)))

(use-package transient
  :custom
  (transient-display-buffer-action '(nil
				     (category . transient)))
  :config
  (transient-bind-q-to-quit))

;;; third-party major modes

