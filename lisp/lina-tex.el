;; -*- lexical-binding: t; -*-
(defun my-latex-word-count ()
  (interactive)
  (if (use-region-p)
      (call-interactively #'count-words-region)
    (shell-command (concat "texcount -brief " (buffer-file-name)))))

;; (use-package browse-file
;;   :load-path "lisp/"
;;   :commands browse-file)

;; (defun lina-insert-noweb-chunk ()
;;   (interactive)
;;   (let* (point-inside
;;          (name (if current-prefix-arg
;;                    (read-string "Name for noweb chunk: ")
;;                  "")))
;;     (unless (bolp)
;;       (end-of-line)
;;       (newline))
;;     (insert "<<" name ">>=\n")
;;     (setq point-inside (point))
;;     (insert "\n@\n")
;;     (goto-char point-inside)))

(autoload #'browse-url-with-browser-kind "browse-url" nil t)
(autoload #'browse-url-file-url "browse-url")

(defvar-local browse-file-last-path nil
  "Last path given to `browse-file'. Can be set locally to set default path.")

(defun browse-file (path)
  "Call external browser to open PATH.

Interactively when `browse-file-last-path' is nil or command is prefixed,
prompt for PATH. Sets `browse-file-last-path' to last successful browsed
file."
  (interactive
   `(,(if (or (null browse-file-last-path) current-prefix-arg)
          (read-file-name "Open file: " nil nil t)
        browse-file-last-path)))
  (let ((proc (browse-url-with-browser-kind
               'external (browse-url-file-url (expand-file-name path)))))
    (when (= (process-exit-status proc) 0)
      (setq-local browse-file-last-path path))))

(use-package tex
  :defines LaTeX-mode-map
  :init
  (defun my-auc-latex-hook ()
    (unless (assoc "multicols" LaTeX-indent-environment-list)
      (push '("multicols" current-indentation) LaTeX-indent-environment-list)))
  :custom
  (TeX-parse-self t)
  (TeX-auto-save t)
  (TeX-master nil)
  (TeX-command-force "LaTeX")
  (TeX-save-query nil)
  (LaTeX-verbatim-environments
   '("verbatim" "verbatim*" "filecontents" "filecontents*" "minted"))
  (LaTeX-verbatim-macros-with-braces '("Sexpr"))
  (TeX-view-program-selection `((output-pdf ,(if (eq system-type 'darwin)
                                                 "Skim"
                                               "open"))))
  (LaTeX-section-hook '(LaTeX-section-heading
                        LaTeX-section-title
                        LaTeX-section-section))
  (reftex-plug-into-AUCTeX t)
  ;; (TeX-command-list '(("Knitr" "Rscript -e \"library(knitr); knit('%t')\""
  ;;                      TeX-run-command nil t :help "Run Knitr")))
  ;; (TeX-file-extensions
  ;;  '("tex" "sty" "cls" "ltx" "texi" "txi" "texinfo" "dtx" "Rnw"))
  :custom-face
  (font-latex-warning-face ((t (:weight normal))))
  :hook (LaTeX-mode . my-auc-latex-hook)
  :hook (TeX-mode . variable-pitch-mode)
  :hook (TeX-mode . turn-on-reftex)
  :bind
  (:map TeX-mode-map
	("M-=" . my-latex-word-count)
        ("C-c C-c" . TeX-command-run-all)
        ("C-c C-a" . TeX-command-master)
        ("C-c C-f" . LaTeX-math-frac)
        ("C-f" . TeX-font)
        ;; ("C-c C-c" . recompile)
        ;; ("C-c C-a" . nil)
        ;; ("C-c C-b" . nil)
        ;; ([remap TeX-next-error] . nil)
        ;; ("C-c C-v" . browse-file)
        ))

(use-package reformatter
  :after tex
  :config
  (reformatter-define latexindent
    :program "latexindent"
    :lighter " Ltxindent")
  ;; :bind (:map LaTeX-mode-map
  ;;             ("C-c f" . latexindent-buffer))
  )

;; (use-package mmm-mode
;;   :disabled t
;;   :init
;;   (defun lina-noweb-mmm-match-submode (front)
;;     (pcase (file-name-extension (buffer-file-name))
;;       ("texw" 'python-mode)      ("Rnw" 'ess-r-mode)
;;       (_ 'prog-mode)))
;;   ;; (defun lina-minted-mmm-match-submode (front)
;;   ;;   (message "%s" front)
;;   ;;   (let (gruppe
;;   ;;         (regexp (rx "\\begin{minted}{" (group-n 1 (+? nonl)) "}")))
;;   ;;     (string-match regexp front)
;;   ;;     (setq gruppe (match-string 1 front))
;;   ;;     (message gruppe)
;;   ;;     (pcase gruppe
;;   ;;       ("java" 'java-mode)
;;   ;;       ("rust" 'rust-ts-mode)
;;   ;;       (x 'prog-mode))))
;;   :custom-face
;;   (mmm-default-submode-face
;;    ((t (:background unspecified :inherit fixed-pitch))))
;;   :config
;;   (setq
;;    mmm-global-classes nil
;;    mmm-mode-ext-classes-alist `((nil ,(rx "." (or "Rnw" "texw") eos) noweb))
;;    mmm-classes-alist `((noweb
;;                         :match-submode lina-noweb-mmm-match-submode
;;                         :face mmm-default-submode-face
;;                         :front ,(rx bol "<<" (* nonl) ">>=\n")
;;                         :back ,(rx bol "@" eol))
;;                        ;; (minted
;;                        ;;  :match-submode lina-minted-mmm-match-submode
;;                        ;;  :face mmm-default-submode-face
;;                        ;;  :front ,(rx "\\begin{minted}{" (+? nonl) "}")
;;                        ;;  :back ,(rx "\\end{minted}"))
;;                        ))
;;   )
