;; -*- lexical-binding: t; -*-
(eval-when-compile
  (require 'use-package))
(require 'bind-key)

(defun latex-word-count ()
  (interactive)
  (if (use-region-p)
      (call-interactively #'count-words-region)
    (shell-command (concat "texcount -brief " (buffer-file-name)))))

(use-package browse-file
  :load-path "lisp/"
  :autoload browse-file)

(use-package tex-mode
  :hook (tex-mode . variable-pitch-mode)
  :bind (:map tex-mode-map
              ("M-=" . latex-word-count)
              ("C-c C-c" . recompile)))

(use-package tex
  :ensure auctex
  :custom
  (TeX-parse-self t)
  (TeX-master nil)
  ;; (TeX-command-list '(("Knitr" "Rscript -e \"library(knitr); knit('%t')\""
  ;;                      TeX-run-command nil t :help "Run Knitr")))
  ;; (TeX-view-program-selection `((output-pdf ,(if (eq system-type 'darwin)
  ;;                                                "Skim"
  ;;                                              "open"))))
  ;; (TeX-file-extensions
  ;;  '("tex" "sty" "cls" "ltx" "texi" "txi" "texinfo" "dtx" "Rnw"))
  :custom-face
  (font-latex-warning-face ((t (:weight normal))))
  :hook (TeX-mode . show-paren-local-mode)
  :hook (TeX-mode . variable-pitch-mode)
  :config
  (setopt LaTeX-indent-environment-list
          (cons '("multicols" current-indentation)
                (eval (car (get 'LaTeX-indent-environment-list
                                'standard-value)))))
  :bind (:map TeX-mode-map
	      ("M-=" . latex-word-count)
              ("C-c C-c" . recompile)
              ("C-c C-a" . nil)
              ("C-c C-b" . nil)
              ("C-c C-v" . browse-file)
              ))

(use-package preview-latex
  :after tex
  :init
  (defun lina-preview-latex-hook ())
  :custom
  (preview-image-type 'dvipng)
  (preview-scale-function 0.8)
  :hook (LaTeX-mode . lina-preview-latex-hook))

(use-package cdlatex)

;; (use-package auctex-latexmk
;;   :disabled t
;;   :ensure
;;   :after (tex)
;;   :functions auctex-latexmk-setup
;;   :custom (auctex-latexmk-inherit-TeX-PDF-mode t)
;;   :config (auctex-latexmk-setup))

(use-package autoinsert
  :config
  (dolist (condition '(plain-tex-mode bibtex-mode latex-mode))
    (assq-delete-all condition auto-insert-alist))
  (define-auto-insert (rx "." (or "tex" "ltx") string-end)
    "latex"))
