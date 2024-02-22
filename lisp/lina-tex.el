;; -*- lexical-binding: t; -*-
(eval-when-compile
  (require 'use-package)
  (require 'bind-key))

(use-package tex
  :ensure auctex
  :defer t
  :init
  (defun latex-word-count ()
    (interactive)
    (if (use-region-p)
        (call-interactively #'count-words-region)
      (shell-command (concat "texcount -brief " (buffer-file-name)))))
  :custom ((TeX-global-PDF-mode t)
           (TeX-auto-save t)
           (TeX-parse-self t))
  :bind (:map TeX-mode-map
	      ("M-=" . #'latex-word-count)))

(use-package auctex-latexmk
  :ensure
  :after (tex)
  :functions auctex-latexmk-setup
  :custom (auctex-latexmk-inherit-TeX-PDF-mode t)
  :config (auctex-latexmk-setup))
