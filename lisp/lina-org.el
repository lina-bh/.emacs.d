;; -*- lexical-binding: t; -*-
(use-package org
  :custom
  (org-adapt-indentation nil)
  (org-link-descriptive nil)
  (org-export-backends '(html latex))
  (org-babel-load-languages '((emacs-lisp . t)
                              (python . t)
                              (R . t)
                              (shell . t)
                              (latex . t)))
  (org-babel-python-command "python3")
  (org-confirm-babel-evaluate nil)
  (org-refile-targets '((nil :maxlevel . 2)))
  (org-html-postamble nil)
  (org-startup-folded 'show2levels)
  (org-format-latex-options
   (list :foreground 'default
         :background "Transparent"
         :scale 2.0
         :html-foreground "Black"
         :html-background "Transparent"
         :html-scale 1.0
         :matchers '("begin" "$1" "$" "$$" "\\(" "\\[")))
  (org-src-lang-modes `(,@(and (fboundp #'LaTeX-mode)
                               (mapcar (lambda (lang)
                                         (cons lang 'LaTeX))
                                       '("latex" "beamer")))
                        ,@(and (treesit-language-available-p 'bash)
                               (mapcar (lambda (lang)
                                         (cons lang 'bash-ts))
                                       '("sh" "shell" "bash")))))
  (org-src-preserve-indentation t)
  (org-src-window-setup 'plain)
  (org-preview-latex-default-process (if (executable-find "dvisvgm")
                                         'dvisvgm
                                       'dvipng))
  (org-latex-classes
   '(("article" "\\documentclass[a4paper,11pt]{article}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))
  (org-latex-default-packages-alist
   '(("AUTO" "inputenc" t ("pdflatex"))
     ("T1" "fontenc" t ("pdflatex"))
     ("" "graphicx" t)
     ("" "amsmath" t)
     ("bookmarks=false,colorlinks=true,urlcolor=blue,linkcolor=,citecolor="
      "hyperref" nil)))
  (org-latex-packages-alist '(("margin=1in" "geometry")
                              ("" "lmodern")
                              ("" "minted")
                              ("british" "babel")))
  (org-latex-src-block-backend 'minted)
  (org-latex-minted-options '(("breaklines" . t)
                              ("ignorelexererrors" . t)
                              ("samepage" . t)))
  (org-latex-remove-logfiles nil)
  (org-latex-pdf-process
   (nreverse
    '("%latex -interaction=nonstopmode -output-directory=%o -shell-escape \
-draftmode %f"
      "%latex -interaction=batchmode -halt-on-error -shell-escape \
-output-directory=%o %f")))
  (org-export-with-smart-quotes t)
  :config
  (defun my-org-hook ()
    (setq-local line-spacing 0.8)
    (variable-pitch-mode)
    (flyspell-mode)
    (face-remap-add-relative 'variable-pitch :family "Times" :height 190))
  :hook (org-mode . my-org-hook)
  :hook (org-babel-after-execute . org-redisplay-inline-images)
  :bind
  ("C-c C-l" . org-store-link)
  (:map org-mode-map
        ("C-c x l" . org-latex-preview)
        ("C-c l" . org-latex-preview)
        ("C-c p" . org-latex-export-to-pdf)
        ("C-c t" . org-babel-tangle))
  (:map org-src-mode-map
        ("C-c C-c" . org-edit-src-exit)))

(use-package export-section
  :bind (:map org-mode-map
              ("C-c s p" . org-latex-export-section-to-pdf)))

(provide 'lina-org)
