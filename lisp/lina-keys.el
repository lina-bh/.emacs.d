;; -*- lexical-binding: t; -*-
(require 'lina-funs)

(bind-key "C-x 2" #'split-and-follow-vertically)
(bind-key "C-x 3" #'split-and-follow-horizontally)

(bind-key "C-k" #'kill-whole-line)
(bind-key "C-w" #'backward-kill-word)

(bind-key "C-u" #'backward-kill-sentence minibuffer-local-map)

(bind-key "C-/" #'comment-line-in-place)

(bind-keys :map emacs-lisp-mode-map
	   ("C-c f" . indent-buffer)
	   ("C-c e f" . indent-last-sexp))

(use-package cua-base
  :init (cua-mode t)
  :bind (:map cua-global-keymap
	      ("C-z" . undo)
	      ("C-y" . nil)
	      ("C-s" . save-buffer)
	      ("C-f" . isearch-forward)
	      ("<C-return>" . nil)
	      ("C-a" . mark-whole-buffer)))

(use-package undo-tree
  :ensure t
  :config (global-undo-tree-mode t)
  :bind (:map cua-global-keymap
	      ("C-z" . undo-tree-undo)
	      ("C-y" . undo-tree-redo)))

(provide 'lina-keys)
