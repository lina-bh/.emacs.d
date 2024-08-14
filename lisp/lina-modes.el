;; -*- lexical-binding: t; -*-
(use-package prog-mode
  :config
  (defun my-prog-mode-hook ()
    (setq-local show-trailing-whitespace t
                truncate-lines t)
    (display-line-numbers-mode)
    (display-fill-column-indicator-mode)
    (show-paren-local-mode))
  :hook
  (prog-mode . my-prog-mode-hook))

(use-package comint
  :custom
  (comint-prompt-read-only t)
  (comint-scroll-to-bottom-on-input 'this)
  :bind (:map comint-mode-map
              ("<up>" . comint-previous-input)
              ("<down>" . comint-next-input)))

(use-package flymake
  :hook ((emacs-lisp-mode sh-base-mode) . flymake-mode))

(use-package text-mode
  :hook (text-mode . visual-line-mode))

(use-package face-remap
  :config
  (defun my-variable-pitch-mode-hook ()
    (setq-local cursor-type (if buffer-face-mode 'bar t)))
  :hook (buffer-face-mode . my-variable-pitch-mode-hook))

(use-package autoinsert
  :demand t
  :custom
  (auto-insert-directory (locate-user-emacs-file "auto-insert/"))
  (auto-insert-alist `((,(rx "." (or "tex" "ltx") string-end) . "latex")
                       ("\\.el\\'" . #'elisp-enable-lexical-binding))))

(use-package treesit-auto
  :if (treesit-available-p)
  :demand t
  :custom
  (global-treesit-auto-mode t)
  (treesit-auto-langs '(java bash yaml json typescript tsx rust))
  :config
  (treesit-auto-install-all)
  (treesit-auto-add-to-auto-mode-alist))

(use-package sh-script
  :custom (sh-basic-offset 2))

(use-package css-mode
  :custom (css-indent-offset 2))

(use-package yaml-ts-mode
  :hook (yaml-ts-mode . display-line-numbers-mode))

(use-package rust-mode
  :defines rust-mode-map
  :custom
  (rust-mode-treesitter-derive t)
  :mode ("\\.rs\\'" . rust-mode)
  :bind (:map rust-mode-map
              ("C-c C-c" . rust-compile)
              ("C-c C-k" . rust-check)
              ("C-c f" . eglot-format)))

;; both of these modes were written by Eric S Raymond and they are both annoying
;; and/or broken. coincidence??

(use-package asm-mode
  :init
  (defun my-asm-mode-hook ()
    (setq-local tab-width 2)
    (when (boundp 'asm-comment-char)
      (local-unset-key (vector asm-comment-char))))
  :hook (asm-mode . my-asm-mode-hook)
  :config (unbind-key ":" 'asm-mode-map))

(use-package make-mode
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
  (defun my-make-hook ()
    (setq-local indent-line-function #'indent-makefile
                tab-always-indent t
                whitespace-style '(face indentation tab-mark))
    (indent-tabs-mode)
    (whitespace-mode))
  :hook (makefile-mode . my-make-hook))
