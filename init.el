;; -*- lexical-binding: t; -*-
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file t)

(add-to-list 'load-path (locate-user-emacs-file "lisp/"))
(load "lina-package")
(load "lina-before")
(load "lina-cmds")
(load "lina-core")
(load "lina-keys")
(load "lina-modes")
(load "lina-compl")
(load "lina-check")
(load "lina-org")
(load "lina-tex")
(load "lina-random")

(pcase system-type
  ;; ('windows-nt (load "lina-w32"))
  ('darwin (load "lina-macos")))
