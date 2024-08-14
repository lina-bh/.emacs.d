;; -*- lexical-binding: t; -*-
(require 'java-skeletons)

(use-package java-ts-mode
  :config
  (defun my-java-hook ()
    (setq-local tab-width 4))
  :hook (java-ts-mode . my-java-hook)
  :bind (:map java-ts-mode-map
              ("C-c f" . eglot-format)
              ("C-c C-c" . project-recompile)
              ("C-c C-k" . java-skeleton-class)
              ("C-c C-m" . java-skeleton-method)
              ("M-RET" . java-javadoc-newline)
              ;; ("C-c ]" . lina-java-close-curly-bracket)
              ))

(use-package eglot-java
  :disabled t
  :functions eglot-java--init
  :load-path "lisp/eglot-java"
  :after eglot
  :demand t
  :custom
  (eglot-java-file-new-ask-type nil)
  :config
  (eglot-java--init))
  ;; :hook (java-ts-mode . eglot-java-mode)
  ;; :bind (:map eglot-java-mode-map
  ;;             ("C-c j n" . #'eglot-java-file-new))

(defun eglot-java-clean-workspace ()
  (interactive)
  (eglot-execute (eglot--current-server-or-lose)
                 '(:title
                   "Clean Workspace"
                   :command
                   "java.clean.workspace")))
