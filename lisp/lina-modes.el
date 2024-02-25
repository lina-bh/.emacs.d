;; -*- lexical-binding: t; -*-
(eval-when-compile
  (require 'use-package)
  (require 'bind-key))

;;; * base hooks & modes
(use-package display-line-numbers
  :custom
  (display-line-numbers-grow-only t)
  (display-line-numbers-width-start 1000))

(use-package display-fill-column-indicator
  :custom
  (display-fill-column-indicator-character ?\u2595) ;; RIGHT ONE EIGHT BLOCK
  (display-fill-column-indicator-column 80))

(use-package prog-mode
  :init
  (defun lina-prog-mode-hook ()
    (setq-local show-trailing-whitespace t
                indent-tabs-mode nil)
    (let ((inhibit-message t))
      (toggle-truncate-lines t)))
  :hook
  (prog-mode . lina-prog-mode-hook)
  (prog-mode . display-line-numbers-mode)
  (prog-mode . display-fill-column-indicator-mode)
  (prog-mode . show-paren-local-mode))

(use-package comint
  :defer t
  :bind (:map comint-mode-map
	      ("<up>" . 'comint-previous-input)
	      ("<down>" . 'comint-next-input)))

(use-package text-mode
  :hook (text-mode . visual-line-mode))

(use-package face-remap
  :init
  (defun lina-variable-pitch-mode-hook ()
    (setq-local cursor-type (if buffer-face-mode 'bar t)))
  :hook (buffer-face-mode . lina-variable-pitch-mode-hook))

;;; * tree sitter
(use-package treesit
  :init
  (defun lina-tree-sitter-repo-for (lang)
    (unless (stringp lang) (setq lang (symbol-name lang)))
    (concat "https://github.com/tree-sitter/tree-sitter-" lang))
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
  (setq treesit-language-source-alist
        `(,@(mapcar (lambda (lang)
                      (list lang (lina-tree-sitter-repo-for lang)))
                    '(java bash))
          (typescript ,(lina-tree-sitter-repo-for "typescript")
                      "master" "typescript/src")
          (tsx ,(lina-tree-sitter-repo-for "typescript")
               "master" "tsx/src")))
  (mapc #'lina-setup-treesit '(java typescript tsx))
  (lina-setup-treesit 'bash 'sh-mode)
  :mode ("\\.ts\\'" . tsx-ts-mode))

;;; * language-specific

;;; ** Emacs Lisp
(use-package elisp-mode
  :init
  (defun lina-elisp-mode-hook ()
    (setq-local outline-regexp (rx (>= 3 ";") space (one-or-more "*")))
    (let ((auto-insert-query nil))
      (auto-insert)))
  :hook (emacs-lisp-mode . lina-elisp-mode-hook)
  :hook (emacs-lisp-mode . outline-minor-mode)
  :bind (:map emacs-lisp-mode-map
	      ("C-c m e" . pp-macroexpand-last-sexp)
	      ("C-c C-c" . eval-buffer)))

(use-package autoinsert
  :config
  (setcdr
   (seq-find (lambda (el)
               (let ((condition (car el)))
                 (and (consp condition)
                      (equal (car condition) "\\.el\\'"))))
             auto-insert-alist)
   (lambda ()
     (insert ";; -*- lexical-binding: t; -*-\n")
     (setq-local lexical-binding t))))

(use-package ielm
  :defer t
  :commands ielm-return
  :init
  (defun ielm-C-c ()
    (interactive)
    (comint-skip-input)
    (ielm-return))
  :bind (:map ielm-map ("C-c C-c" . #'ielm-C-c)))

;;; ** assorted
;; TODO fix C mode

(use-package sh-script
  :defer t
  :custom (sh-basic-offset 2)
  :hook (sh-base-mode . flymake-mode))

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

(use-package yaml-mode
  :disabled t
  :hook (yaml-mode . display-line-numbers-mode))

(use-package outline
  :custom
  (outline-minor-mode-cycle t)
  (outline-blank-line t)
  (outline-minor-mode-prefix (kbd "C-c C-o"))
  (outline-minor-mode-use-buttons 'in-margins))

;;; ** unused

(use-package racket-mode
  :disabled t
  :bind (:map racket-mode-map
	      ("C-c C-c" . racket-run)))

(use-package plantuml-mode
  :disabled t
  :load-path "lisp/plantuml-mode"
  :custom (plantuml-default-exec-mode 'jar)
  :config (add-to-list 'plantuml-jar-args "-tpng"))

;; TODO bison-mode fix tab properly
