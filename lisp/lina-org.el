;; -*- lexical-binding: t; -*-
(eval-when-compile
  (require 'use-package))
(eval-and-compile
  (require 'vc-use-package))
(require 'bind-key)

(use-package unidecode
  :ensure
  :demand t)

(use-package org
  :functions
  org-latex-compile
  org-latex-export-section-to-pdf
  org-export-to-file
  org-find-exact-headline-in-buffer
  org-get-outline-path
  :init
  (defun org-latex-export-section-to-pdf ()
    (interactive)
    (if-let ((heading (nth 0 (org-get-outline-path t)))
             (position (org-find-exact-headline-in-buffer
                        heading
                        (current-buffer)
                        t))
             (outfile (concat (unidecode-sanitize heading) ".tex")))
        (save-excursion
          (goto-char position)
          (org-export-to-file
           'latex outfile nil t nil nil nil #'org-latex-compile))
      (funcall-interactively #'org-latex-export-to-pdf)))
  :custom
  (org-adapt-indentation nil)
  (org-link-descriptive nil)
  (org-export-backends '(html latex))
  (org-modules '(ox-latex))
  (org-babel-load-languages '((emacs-lisp . t)
			      (python . t)
			      (R . t)
			      (shell . t)
                              (latex . t)))
  (org-src-preserve-indentation t)
  (org-confirm-babel-evaluate nil)
  (org-refile-targets '((nil :maxlevel . 2)))
  (org-html-postamble nil)
  (org-latex-classes
   '(("article" "\\documentclass[a4paper,11pt]{article}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))
  (org-latex-default-packages-alist
   '(("AUTO" "inputenc" t
      ("pdflatex"))
     ("T1" "fontenc" t
      ("pdflatex"))
     ("" "graphicx" t)
     ("" "amsmath" t)
     ("bookmarks=false,colorlinks=true,urlcolor=blue,linkcolor=,citecolor="
      "hyperref" nil)))
  (org-latex-packages-alist '(("margin=1in" "geometry")
                              ("" "lmodern")))
  (org-latex-remove-logfiles nil)
  (org-latex-pdf-process
   (nreverse
    '("%latex -interaction=nonstopmode -output-directory=%o -draftmode %f"
      "%latex -interaction=nonstopmode -output-directory=%o %f")))
  :config
  (setopt
   org-src-lang-modes
   (let* ((default-modes (eval (car (get 'org-src-lang-modes 'standard-value))))
          (make-function (lambda (value)
                           (lambda (key)
                             (setf (alist-get key default-modes) value)))))
     (mapc (funcall make-function 'bash-ts)
           '("bash" "shell" "sh"))
     (mapc (funcall make-function 'LaTeX)
           '("beamer" "latex"))
     default-modes))
  :hook (org-mode . variable-pitch-mode)
  :hook (org-mode . flyspell-mode)
  :hook (org-babel-after-execute . org-redisplay-inline-images)
  :bind (:map org-mode-map
              ("C-c p" . #'org-latex-export-to-pdf)
              ("C-c s p" . #'org-latex-export-section-to-pdf)
              ("C-c t" . #'org-babel-tangle)))

(use-package htmlize
  :ensure
  :after (ox-html)
  :demand t)

;; engravings give you no tactical advantage whatsoever.
(use-package engrave-faces
  :pin gnu-devel
  :ensure
  :demand t
  :after (ox-latex)
  :custom (org-latex-src-block-backend 'engraved))

(use-package phscroll
  :vc (:fetcher github :repo misohena/phscroll)
  :after (org))

(use-package org-wc
  :disabled t
  :load-path "site-lisp/org-wc"
  :after org
  :bind (:map org-mode-map
	      ("C-c w" . org-wc-display)))
