;; -*- lexical-binding: t; -*-
(autoload 'export-section "export-section" nil t)
(autoload 'org-redisplay-inline-images "org" nil t)

(setopt org-adapt-indentation nil
        org-link-descriptive nil
        org-export-backends '(html latex)
        org-babel-load-languages '((emacs-lisp . t)
                                   (python . t)
                                   (R . t)
                                   (shell . t)
                                   ;; (latex . t)
                                   )
        org-babel-python-mode "python3"
        org-confirm-babel-evaluate nil
        org-refile-targets '((nil :maxlevel . 2))
        org-html-postamble nil
        org-startup-folded 'show2levels
        org-format-latex-options
        (list :foreground 'default
              :background "Transparent"
              :scale 2.0
              :html-foreground "Black"
              :html-background "Transparent"
              :html-scale 1.0
              :matchers '("begin" "$1" "$" "$$" "\\(" "\\["))
        org-src-lang-modes `(,@(and (fboundp #'LaTeX-mode)
                                    (mapcar (lambda (lang)
                                              (cons lang 'LaTeX))
                                            '("latex" "beamer")))
                             ,@(and (treesit-language-available-p 'bash)
                                    (mapcar (lambda (lang)
                                              (cons lang 'bash-ts))
                                            '("sh" "shell" "bash"))))
        org-src-preserve-indentation t
        org-src-window-setup 'plain
        org-latex-classes
        '(("article" "\\documentclass[a4paper,11pt]{article}"
           ("\\section{%s}" . "\\section*{%s}")
           ("\\subsection{%s}" . "\\subsection*{%s}")
           ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
           ("\\paragraph{%s}" . "\\paragraph*{%s}")
           ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
        org-latex-default-packages-alist
        '(("AUTO" "inputenc" t ("pdflatex"))
          ("T1" "fontenc" t ("pdflatex"))
          ("" "graphicx" t)
          ("" "amsmath" t)
          ("bookmarks=false,colorlinks=true,urlcolor=blue,linkcolor=,citecolor="
           "hyperref" nil))
        org-latex-packages-alist '(("margin=1in" "geometry")
                                   ("" "lmodern")
                                   ("" "minted")
                                   ("british" "babel"))
        org-latex-src-block-backend 'minted
        org-latex-minted-options '(("breaklines" . t)
                                   ("ignorelexererrors" . t)
                                   ("samepage" . t))
        org-latex-remove-logfiles nil
        org-latex-pdf-process
        (nreverse
         '("%latex -interaction=nonstopmode -output-directory=%o -shell-escape \
-draftmode %f"
           "%latex -interaction=batchmode -halt-on-error -shell-escape \
-output-directory=%o %f"))
        org-export-with-smart-quotes t
        org-agenda-window-setup 'current-window)
;; (defvar org-preview-latex-default-process (if (executable-find "dvisvgm")
;;                                               'dvisvgm
;;                                             'dvipng))
(defun my-org-hook ()
  (setq-local line-spacing 0.2)
  (variable-pitch-mode)
  (flyspell-mode)
  (face-remap-add-relative 'variable-pitch
                           :family "Liberation Serif"
                           :height 120))
(add-hook 'org-mode-hook #'my-org-hook)
(add-hook 'org-agenda-mode-hook #'my-org-hook)

(add-hook 'org-babel-after-execute-hook #'org-redisplay-inline-images)
(bind-keys ("C-c C-l" . org-store-link))
(with-eval-after-load 'org
  (bind-keys :map org-mode-map
             ("C-c l" . org-latex-preview)
             ("C-c p" . org-latex-export-to-pdf)
             ("C-c t" . org-babel-tangle)
             ("C-c s p" . org-latex-export-section-to-pdf)
             ("C-t" . org-todo))
  (bind-keys :map org-src-mode-map
             ("C-c C-c" . org-edit-src-exit)))

(provide 'my-org)
