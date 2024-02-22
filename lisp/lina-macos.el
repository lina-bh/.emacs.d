;; -*- lexical-binding: t; -*-
(when (display-graphic-p)
  (mapc (lambda (face)
	  (set-face-attribute face nil
			      :family "Monaco"
			      :height 115))
	'(default fixed-pitch))
  (set-face-attribute 'fill-column-indicator nil
                      :family "Menlo")
  (set-face-attribute 'tab-line nil
		      :family "Helvetica")
  (set-face-attribute 'variable-pitch nil
		      :family "Lucida Grande"
		      :height 140))
(setopt ns-command-modifier 'super
	dired-use-ls-dired nil
        mouse-wheel-flip-direction t)
;; (defalias 'man 'woman)

(use-package dash-at-point
  :ensure
  :bind (("C-c d" . dash-at-point)))

;; (use-package man
;;   :defer t
;;   :custom (manual-program "gman"))
