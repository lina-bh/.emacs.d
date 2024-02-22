;; -*- lexical-binding: t; -*-
(eval-when-compile
  (require 'use-package)
  (require 'bind-key))

(use-package prog-mode
  :init
  (defun lina-prog-mode-hook ()
    (setq-local show-trailing-whitespace t
                indent-tabs-mode nil)
    (let ((inhibit-message t))
      (toggle-truncate-lines t)))
  :hook (prog-mode . lina-prog-mode-hook))

(use-package treesit
  :init
  (defun lina-tree-sitter-repo-for (lang)
    (let ((lang (if (not (stringp lang))
                    (symbol-name lang)
                  lang)))
      (concat "https://github.com/tree-sitter/tree-sitter-"
              lang)))
  (defun lina-setup-treesit (lang &optional orig-major-mode)
    (unless (assq lang treesit-language-source-alist)
      (error "don't know language %s" lang))
    (unless (treesit-language-available-p lang)
      (treesit-install-language-grammar lang))
    (let* ((lang-name (symbol-name lang))
	   (orig-major-mode (or orig-major-mode
				(intern (format "%s-mode" lang-name))))
	   (ts-major-mode (intern (format "%s-ts-mode" lang-name))))
      (when (and
             (fboundp ts-major-mode)
             (fboundp orig-major-mode))
	(setf (alist-get orig-major-mode major-mode-remap-alist)
	      ts-major-mode))))
  :config
  (setopt treesit-language-source-alist
          (append
           (mapcar (lambda (lang)
                     (list lang (lina-tree-sitter-repo-for lang)))
                   '(java bash))
           (list
            (list 'typescript (lina-tree-sitter-repo-for "typescript")
                  "master" "typescript/src")
            (list 'tsx (lina-tree-sitter-repo-for "typescript")
                  "master" "tsx/src"))))
  (mapc #'lina-setup-treesit '(java typescript tsx))
  (lina-setup-treesit 'bash 'sh-mode)
  :mode ("\\.ts\\'" . tsx-ts-mode))

(use-package comint
  :bind (:map comint-mode-map
	      ("<up>" . 'comint-previous-input)
	      ("<down>" . 'comint-next-input)))

;;; ** Emacs Lisp


(use-package elisp-mode
  :bind (:map emacs-lisp-mode-map
	      ("C-c m e" . #'pp-macroexpand-last-sexp)
	      ("C-c C-c" . #'eval-buffer)))

(use-package ielm
  :defer t
  :commands ielm-return
  :init
  (defun ielm-C-c ()
    (interactive)
    (comint-skip-input)
    (ielm-return))
  :bind (:map ielm-map
              ("C-c C-c" . #'ielm-C-c)))

(use-package cc-mode
  :defer t
  :init
  (defun lina-c-mode-hook ()
    (setq-local indent-tabs-mode t))
  :hook (c-mode . lina-c-mode-hook)
  ;; TODO move to using default
  :config (setf (alist-get 'other c-default-style) "linux"))

(use-package sh-script
  :custom (sh-basic-offset 2))
(use-package flymake
  :hook sh-base-mode)

(use-package asm-mode
  :defer t
  :init
  (defun lina-asm-mode-hook ()
    (setq-local tab-width 2)
    (when (boundp 'asm-comment-char)
      (local-unset-key (vector asm-comment-char))))
  :hook (asm-mode . lina-asm-mode-hook)
  :config (unbind-key ":" 'asm-mode-map))

(use-package make-mode
  :init
  (defun lina-makefile-hook ()
    (setq-local indent-tabs-mode t))
  :hook (makefile-mode . lina-makefile-hook))

(use-package js
  :defer t
  :custom (js-indent-level 2))

(use-package sql
  :defer t
  :custom (sql-product 'sqlite))

(use-package rust-mode
  :defer t
  :ensure)

(use-package yaml-mode
  :ensure
  :hook (yaml-mode . display-line-numbers-mode))

(use-package outline
  :hook (emacs-lisp-mode . outline-minor-mode))

;;; unused

(use-package racket-mode
  :disabled t
  :bind (:map racket-mode-map
	      ("C-c C-c" . racket-run)))

(use-package plantuml-mode
  :disabled t
  :load-path "lisp/plantuml-mode"
  :custom (plantuml-default-exec-mode 'jar)
  :config (add-to-list 'plantuml-jar-args "-tpng"))

;;; TODO bison-mode fix tab properly
