;; -*- lexical-binding: t; -*-
(eval-when-compile
  (require 'use-package))
(require 'bind-key)

;; (defun treesit-ensure (lang)
;;   (unless (treesit-language-available-p lang)
;;     (treesit-install-language-grammar lang)))

(defun github-tree-sitter (lang)
  (concat "https://github.com/tree-sitter/tree-sitter-" lang))

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
  :commands comint-skip-input comint-next-input comint-previous-input
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

(use-package autoinsert
  :custom (auto-insert-directory (locate-user-emacs-file "auto-insert/"))
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

(use-package elisp-mode
  :init
  (defun lina-elisp-mode-hook ()
    (setq-local outline-regexp (rx (>= 3 ";") space (one-or-more "*")))
    (require 'autoinsert)
    (let ((auto-insert-query nil))
      (auto-insert)))
  :hook (emacs-lisp-mode . lina-elisp-mode-hook)
  :bind (:map emacs-lisp-mode-map
	      ("C-c C-c" . eval-buffer)
              ("C-c C-p" . pp-macroexpand-last-sexp)))

(use-package paredit
  :hook (emacs-lisp-mode . paredit-mode))

(use-package ielm
  :commands ielm-return ielm-C-c
  :init
  (defun ielm-C-c ()
    (interactive)
    (comint-skip-input)
    (ielm-return))
  :bind (:map ielm-map ("C-c C-c" . ielm-C-c)))

(use-package treesit
  :init (defvar treesit-language-source-alist nil))

(use-package java-ts-mode
  :after treesit
  :init
  (push `(java ,(github-tree-sitter "java")) treesit-language-source-alist)
  (when (treesit-language-available-p 'java)
    (push '(java-mode . java-ts-mode) major-mode-remap-alist)))

(use-package rust-ts-mode
  :after treesit
  :init
  (push `(rust ,(github-tree-sitter "rust")) treesit-language-source-alist))

(use-package sh-script
  :config (setq-default sh-basic-offset 2))
(use-package sh-script
  :after treesit
  :init
  (push `(bash ,(github-tree-sitter "bash")) treesit-language-source-alist)
  (when (treesit-language-available-p 'bash)
    (push '(sh-mode . bash-ts-mode) major-mode-remap-alist))
  :hook (sh-base-mode . flymake-mode))

(use-package js
  :config (setq-default js-indent-level 2))
(use-package typescript-ts-mode
  :after treesit
  :init
  (setq treesit-language-source-alist
        (append `((typescript ,(github-tree-sitter "typescript")  "master"
                              "typescript/src")
                  (tsx ,(github-tree-sitter "typescript") "master" "tsx/src"))
                treesit-language-source-alist))
  :mode ("\\.ts\\'" . typescript-ts-mode))

(use-package yaml-ts-mode
  :after treesit
  :init
  (push `(yaml ,(github-tree-sitter "yaml")) treesit-language-source-alist)
  :hook (yaml-ts-mode . display-line-numbers-mode)
  :mode ("\\.yaml\\'" . yaml-ts-mode))

(use-package asm-mode
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

(use-package ess-site
  ;; :ensure ess
  :init (setq ess-ask-for-ess-directory nil
              ess-use-flymake nil))

;; make this dir-local instead.
;; (use-package sql
;;   :custom (sql-product 'sqlite))

(use-package outline
  :config
  (setq-default outline-minor-mode-cycle t
                outline-blank-line t
                outline-minor-mode-prefix (kbd "C-c C-o")
                outline-minor-mode-use-buttons 'in-margins))

;; (use-package racket-mode
;;   :disabled t
;;   :bind (:map racket-mode-map
;; 	      ("C-c C-c" . racket-run)))

;; (use-package plantuml-mode
;;   :disabled t
;;   :load-path "lisp/plantuml-mode"
;;   :custom (plantuml-default-exec-mode 'jar)
;;   :config (add-to-list 'plantuml-jar-args "-tpng"))

;; (defun lina/bison-mode-hook ()
;;   (electric-indent-local-mode 0))
;; (defun lina/insert-tab ()
;;   (interactive)
;;   (insert-tab))
;; (use-package bison-mode
;;   :disabled t
;;   :hook ((bison-mode . lina/bison-mode-hook)
;; 	 (flex-mode . lina/bison-mode-hook))
;;   :bind (:map flex-mode-map
;; 	      ("<tab>" . #'lina/insert-tab))
;;   :bind	(:map bison-mode-map
;; 	      ("<tab>" . #'lina/insert-tab)))

;; (defun restart-python ()
;;   (interactive)
;;   (let ((buffer (get-buffer "*Python*"))
;; 	(kill-buffer-query-functions nil))
;;     (when buffer
;;       (kill-buffer buffer)))
;;   (call-interactively #'run-python))

;; (defun restart-python-send-current ()
;;   (interactive)
;;   (restart-python)
;;   (sleep-for 0 1)
;;   (save-some-buffers t
;; 		     (lambda ()
;; 		       (string= (file-name-extension buffer-file-name) "py")))
;;   (call-interactively 'python-shell-send-file))

;; (defun python-tab ()
;;   (interactive)
;;   (let ((initial-point (point)))
;;     (if (use-region-p)
;; 	(save-excursion
;; 	  (move-to-column 0)
;; 	  (call-interactively 'python-indent-shift-right))
;;       (call-interactively 'python-indent-shift-right))
;;     (when (= (point) initial-point)
;;       (call-interactively 'indent-for-tab-command))
;;     (when (= (point) initial-point)
;;       (indent-line-to python-indent-offset))))

;; (use-package python
;;   :defer t
;;   :custom ((python-shell-interpreter-args "-i -q"))
;;   :bind ((:map python-mode-map
;; 	       ("C-c C-l" . 'restart-python-send-current)
;; 	       ;; ("<tab>" . 'python-tab)
;; 	       ;; ("<backtab>" . 'python-indent-shift-left))
;; 	       )))
