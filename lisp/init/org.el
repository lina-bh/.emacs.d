;; -*- lexical-binding: t; -*-
(use-package org
  :ensure t
  :custom
  (org-adapt-indentation nil)
  (org-agenda-window-setup 'current-window)
  (org-export-backends '(html latex md))
  (org-export-with-smart-quotes t)
  (org-html-doctype "html5")
  (org-html-postamble nil)
  (org-link-descriptive nil)
  (org-refile-targets '((nil :maxlevel . 2)))
  (org-startup-folded 'show2levels)
  (org-support-shift-select t)
  ;; src
  (org-confirm-babel-evaluate nil)
  (org-src-preserve-indentation t)
  (org-src-window-setup 'plain)
  (org-src-lang-modes `(,@(and (fboundp #'LaTeX-mode)
                               (mapcar (lambda (lang)
                                         (cons lang 'LaTeX))
                                       '("latex" "beamer")))
                        ,@(and (treesit-language-available-p 'bash)
                               (mapcar (lambda (lang)
                                         (cons lang 'bash-ts))
                                       '("sh" "shell" "bash")))))
  (org-babel-load-languages '((emacs-lisp . t)
                              (python . t)
                              (R . t)
                              (shell . t)
                              ;; (latex . t)
                              ))
  ;; latex
  (org-latex-classes '(("article" "\\documentclass[a4paper,11pt]{article}"
                        ("\\section{%s}" . "\\section*{%s}")
                        ("\\subsection{%s}" . "\\subsection*{%s}")
                        ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                        ("\\paragraph{%s}" . "\\paragraph*{%s}")
                        ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))
  (org-latex-default-packages-alist
   '(("AUTO" "inputenc" t ("pdflatex"))
     ("T1" "fontenc" t ("pdflatex"))
     ("" "fontspec" t ("lualatex"))
     ("" "graphicx" t)
     ("" "amsmath" t)
     ("bookmarks=false,colorlinks=true,urlcolor=blue,linkcolor=,citecolor="
      "hyperref" nil)))
  (org-latex-packages-alist '(("margin=1in" "geometry")
                              ("" "lmodern")
                              ("" "minted")
                              ("british" "babel")))
  (org-format-latex-options '(
                              :foreground 'default
                              :background "Transparent"
                              :scale 2.0
                              :html-foreground "Black"
                              :html-background "Transparent"
                              :html-scale 1.0
                              :matchers ("begin" "$1" "$" "$$" "\\(" "\\["))
                            )
  (org-latex-src-block-backend 'minted)
  (org-latex-minted-options '(
                              ;; ("breaklines" . t)
                              ;; ("ignorelexererrors" . t)
                              ;; ("samepage" . t)
                              ))
  (org-latex-remove-logfiles nil)
  (org-latex-pdf-process
   '("%latex -interaction=nonstopmode -output-directory=%o -shell-escape \
   -draftmode %f"
     "%latex -interaction=batchmode -halt-on-error -shell-escape \
   -output-directory=%o %f")
   )
  :config
  (setq-mode-local org-mode line-spacing 0.2)
  (defun my-org-hook ()
    (variable-pitch-mode)
    (flyspell-mode)
    (face-remap-add-relative 'variable-pitch
                             :family "Liberation Serif"
                             :height 120))
  (defun org-insert-today ()
    (interactive)
    (org-insert-timestamp (current-time) nil t))
  :hook
  ((org-mode org-agenda-mode) . my-org-hook)
  (org-babel-after-execute . org-redisplay-inline-images)
  :bind
  (:map org-mode-map
        ("C-c l" . org-latex-preview)
        ("C-c p" . org-latex-export-to-pdf)
        ("C-c t" . org-babel-tangle)
        ("C-c s p" . org-latex-export-section-to-pdf)
        ("C-c '" . org-edit-special)
        ("C-c x" . org-babel-execute-buffer)
        ("C-c C-c" . org-babel-execute-src-block)
        ("C-c ." . org-insert-today))
  (:map org-src-mode-map
        ("C-c C-c" . org-edit-src-exit)))
