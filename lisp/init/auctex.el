(use-package tex
  :ensure auctex
  :defines LaTeX-mode-map
  :custom
  (TeX-parse-self t)
  (TeX-auto-save t)
  (TeX-master 'shared)
  (TeX-save-query nil)
  (LaTeX-verbatim-environments
   '("verbatim"
     "verbatim*"
     "filecontents"
     "filecontents*"
     "minted"))
  (LaTeX-indent-environment-list
   '(("multicols" current-indentation) ("verbatim" current-indentation)
     ("verbatim*" current-indentation) ("filecontents" current-indentation)
     ("filecontents*" current-indentation) ("tabular" LaTeX-indent-tabular)
     ("tabular*" LaTeX-indent-tabular) ("array" LaTeX-indent-tabular)
     ("eqnarray" LaTeX-indent-tabular) ("eqnarray*" LaTeX-indent-tabular)
     ("align" LaTeX-indent-tabular) ("align*" LaTeX-indent-tabular)
     ("aligned" LaTeX-indent-tabular) ("alignat" LaTeX-indent-tabular)
     ("alignat*" LaTeX-indent-tabular) ("alignedat" LaTeX-indent-tabular)
     ("xalignat" LaTeX-indent-tabular) ("xalignat*" LaTeX-indent-tabular)
     ("xxalignat" LaTeX-indent-tabular) ("flalign" LaTeX-indent-tabular)
     ("flalign*" LaTeX-indent-tabular) ("split" LaTeX-indent-tabular)
     ("matrix" LaTeX-indent-tabular) ("pmatrix" LaTeX-indent-tabular)
     ("bmatrix" LaTeX-indent-tabular) ("Bmatrix" LaTeX-indent-tabular)
     ("vmatrix" LaTeX-indent-tabular) ("Vmatrix" LaTeX-indent-tabular)
     ("smallmatrix" LaTeX-indent-tabular) ("cases" LaTeX-indent-tabular)
     ("longtable" LaTeX-indent-tabular) ("longtable*" LaTeX-indent-tabular)
     ("matrix*" LaTeX-indent-tabular) ("pmatrix*" LaTeX-indent-tabular)
     ("bmatrix*" LaTeX-indent-tabular) ("Bmatrix*" LaTeX-indent-tabular)
     ("vmatrix*" LaTeX-indent-tabular) ("Vmatrix*" LaTeX-indent-tabular)
     ("smallmatrix*" LaTeX-indent-tabular) ("psmallmatrix" LaTeX-indent-tabular)
     ("psmallmatrix*" LaTeX-indent-tabular) ("bsmallmatrix" LaTeX-indent-tabular)
     ("bsmallmatrix*" LaTeX-indent-tabular) ("vsmallmatrix" LaTeX-indent-tabular)
     ("vsmallmatrix*" LaTeX-indent-tabular) ("Vsmallmatrix" LaTeX-indent-tabular)
     ("Vsmallmatrix*" LaTeX-indent-tabular) ("dcases" LaTeX-indent-tabular)
     ("dcases*" LaTeX-indent-tabular) ("rcases" LaTeX-indent-tabular)
     ("rcases*" LaTeX-indent-tabular) ("drcases" LaTeX-indent-tabular)
     ("drcases*" LaTeX-indent-tabular) ("cases*" LaTeX-indent-tabular)
     ("stabular" LaTeX-indent-tabular) ("stabular*" LaTeX-indent-tabular)
     ("supertabular" LaTeX-indent-tabular) ("supertabular*" LaTeX-indent-tabular)
     ("mpsupertabular" LaTeX-indent-tabular)
     ("mpsupertabular*" LaTeX-indent-tabular) ("tblr" LaTeX-indent-tabular)
     ("longtblr" LaTeX-indent-tabular) ("talltblr" LaTeX-indent-tabular)
     ("booktabs" LaTeX-indent-tabular) ("+array" LaTeX-indent-tabular)
     ("+matrix" LaTeX-indent-tabular) ("+bmatrix" LaTeX-indent-tabular)
     ("+Bmatrix" LaTeX-indent-tabular) ("+pmatrix" LaTeX-indent-tabular)
     ("+vmatrix" LaTeX-indent-tabular) ("+Vmatrix" LaTeX-indent-tabular)
     ("+cases" LaTeX-indent-tabular) ("tabularx" LaTeX-indent-tabular)
     ("tabulary" LaTeX-indent-tabular) ("xltabular" LaTeX-indent-tabular)
     ("xtabular" LaTeX-indent-tabular) ("xtabular*" LaTeX-indent-tabular)
     ("mpxtabular" LaTeX-indent-tabular) ("mpxtabular*" LaTeX-indent-tabular)
     ("displaymath") ("equation") ("picture") ("tabbing") ("gather") ("gather*")
     ("gathered") ("equation*") ("multline") ("multline*") ("macrocode")
     ("macrocode*")))
  (LaTeX-verbatim-macros-with-braces '("Sexpr"))
  ;; (TeX-view-program-list '(("Okular"
  ;;                           ("org.kde.okular --unique %o"
  ;;                            (mode-io-correlate "#src:%n%a"))
  ;;                           "okular")))
  (TeX-view-program-selection `((output-pdf ,(cond
                                              ((eq system-type 'darwin)
                                               "Skim")
                                              ((eq system-type 'gnu/linux)
                                               "Okular")))))
  (LaTeX-section-hook '(LaTeX-section-heading
                        LaTeX-section-title
                        LaTeX-section-section))
  ;; (reftex-plug-into-AUCTeX t)
  :custom-face
  (font-latex-warning-face ((t (:weight normal))))
  :config
  ;; (setq-mode-local LaTeX-mode TeX-command-force "LaTeX")
  :hook (TeX-mode . variable-pitch-mode)
  ;; :hook (TeX-mode . turn-on-reftex)
  :bind
  (:map TeX-mode-map
        ("M-=" . count-words)
        ("C-c C-c" . TeX-command-run-all)
        ("C-c C-a" . TeX-command-master)
        ("C-c C-f" . LaTeX-math-frac)
        ("C-f" . TeX-font)))
