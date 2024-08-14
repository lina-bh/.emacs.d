;; -*- lexical-binding: t; -*-
(use-package polymode
  :init
  (defun lina-poly-inner-hook ()
    (show-paren-local-mode -1)
    (display-fill-column-indicator-mode -1))
  :custom
  (polymode-move-these-minor-modes-from-old-buffer '(visual-line-mode
                                                     tab-line-mode))
  :hook (polymode-init-inner . lina-poly-inner-hook))

(use-package poly-noweb
  :init
  (defun lina-poly-minted-mode-matcher ()
    (pcase (buffer-substring-no-properties (point) (- (pos-eol) 1))
      ("java" 'java-ts-mode)
      ("rust" 'rust-ts-mode)
      (_ 'poly-fallback-mode)))
  :config
  (oset poly-latex-hostmode mode 'LaTeX-mode)
  (oset poly-noweb-innermode adjust-face '(:inherit fixed-pitch))
  (oset poly-noweb-polymode keylist nil)

  (define-innermode lina-poly-pweave-innermode poly-noweb-innermode
    :mode 'python-mode)
  (define-polymode lina-poly-pweave-mode poly-noweb-mode
    :lighter " PM-Pw"
    :innermodes '(lina-poly-pweave-innermode))

  (define-auto-innermode lina-poly-minted-auto-innermode nil "Not documented."
    :adjust-face '(:inherit fixed-pitch)
    :head-matcher (cons (rx "\\begin{minted}{" (group (+? nonl)) "}\n") 1)
    :tail-matcher "\\\\end{minted}"
    :mode-matcher #'lina-poly-minted-mode-matcher
    :head-mode 'host
    :tail-mode 'host)
  (define-polymode lina-poly-minted-mode poly-latex-root-polymode
    :hostmode 'poly-noweb-latex-hostmode
    :innermodes '(lina-poly-minted-auto-innermode))
  )
