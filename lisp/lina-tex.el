;; -*- lexical-binding: t; -*-

(use-package tex-mode
  :config (setq-default tex-compile-commands '(("latexmk -pdf -norc" t nil))))
