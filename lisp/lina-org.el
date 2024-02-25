;; -*- lexical-binding: t; -*-
(eval-when-compile
  (require 'use-package)
  (require 'bind-key))

(defun lina-standard-value (sym)
  (eval (car (get sym 'standard-value))))

(defun alist-string-cdr-get (key alist)
  (if alist
      (let ((x (car alist))
	    (rest (cdr alist)))
	(if (string-equal key (cadr x))
	    x
	  (alist-string-cdr-get key rest)))
    nil))

(use-package unidecode
  :ensure
  :defer t)

(use-package org
  :ensure
  :pin gnu
  :defer t
  :functions (org-latex-compile
              org-latex-export-section-to-pdf
              org-export-to-file
              org-find-exact-headline-in-buffer
              org-get-outline-path)
  :init
  (require 'unidecode)
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
  (org-startup-folded t)
  (org-export-backends '(html latex))
  (org-modules '(ox-latex))
  (org-babel-load-languages '((emacs-lisp . t)
			      (python . t)
			      (R . t)
			      (shell . t)
                              (latex . t)))
  (org-src-preserve-indentation t)
  (org-confirm-babel-evaluate nil)
  (org-refile-targets '((nil
                         :maxlevel . 2)))
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
     ;; ("" "longtable" nil)
     ;; ("" "wrapfig" nil)
     ;; ("" "rotating" nil)
     ;; ("normalem" "ulem" t)
     ("" "amsmath" t)
     ;; ("" "amssymb" t)
     ;; ("" "capt-of" nil)
     ("bookmarks=false,colorlinks=true,urlcolor=blue,linkcolor=,citecolor=" "hyperref" nil)))
  (org-latex-packages-alist '(("margin=1in" "geometry")
                              ("" "lmodern")))
  (org-latex-remove-logfiles nil)
  (org-latex-pdf-process
   '("%latex -interaction=nonstopmode -output-directory=%o %f"
     "%latex -interaction=nonstopmode -output-directory=%o -draftmode %f"))
  :config
  (setopt
   org-src-lang-modes
   (let* ((default-modes (lina-standard-value 'org-src-lang-modes))
          (make-function (lambda (value)
                           (lambda (key)
                             (setf (alist-get key default-modes) value)))))
     (mapc (funcall make-function 'bash-ts)
           '("bash" "shell" "sh"))
     (mapc (funcall make-function 'LaTeX)
           '("beamer" "latex"))
     default-modes))
  :hook (org-mode . variable-pitch-mode)
  :hook (org-babel-after-execute . org-redisplay-inline-images)
  :bind (:map org-mode-map
              ("C-c p" . #'org-latex-export-to-pdf)
              ("C-c s p" . #'org-latex-export-section-to-pdf)
              ("C-c t" . #'org-babel-tangle)))

(use-package htmlize
  :ensure
  :after (ox-html))

;; engravings give you no tactical advantage whatsoever.
(use-package engrave-faces
  :preface
  (unless (package-installed-p 'engrave-faces)
    (package-vc-install "https://github.com/tecosaur/engrave-faces"))
  :after (ox-latex)
  :custom (org-latex-src-block-backend 'engraved))

(use-package phscroll
  :preface
  (unless (package-installed-p 'phscroll)
    (package-vc-install "https://github.com/misohena/phscroll"))
  :after (org))

(use-package org-wc
  :disabled t
  :load-path "site-lisp/org-wc"
  :after org
  :bind (:map org-mode-map
	      ("C-c w" . org-wc-display)))
