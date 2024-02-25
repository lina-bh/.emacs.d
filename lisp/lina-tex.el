;; -*- lexical-binding: t; -*-
(eval-and-compile
  (require 'lina-package))

(use-package tex
  :straight auctex
  :defer t
  :init
  (defun latex-word-count ()
    (interactive)
    (if (use-region-p)
        (call-interactively #'count-words-region)
      (shell-command (concat "texcount -brief " (buffer-file-name)))))
  :custom
  (TeX-global-PDF-mode t)
  (TeX-parse-self t)
  :custom-face
  (font-latex-warning-face ((t (:weight normal))))
  :config
  (when (eq system-type 'darwin)
    (setopt TeX-view-program-selection '((output-pdf "Skim")
                                         (output-dvi "open")
                                         (output-html "open"))))
  :hook (TeX-mode . buffer-face-mode)
  :hook (TeX-mode . show-paren-local-mode)
  :bind (:map TeX-mode-map
	      ("M-=" . latex-word-count)))

(use-package auctex-latexmk
  :disabled t
  :ensure
  :after (tex)
  :functions auctex-latexmk-setup
  :custom (auctex-latexmk-inherit-TeX-PDF-mode t)
  :config (auctex-latexmk-setup))

(use-package autoinsert
  :config
  (mapc (lambda (condition)
          (assq-delete-all condition auto-insert-alist))
        '(plain-tex-mode bibtex-mode latex-mode))
  (define-auto-insert (rx "." (or "tex" "ltx") string-end)
    "latex"))
