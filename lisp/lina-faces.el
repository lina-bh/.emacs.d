;; -*- lexical-binding: t; -*-

(set-face-attribute 'fill-column-indicator nil
		    :background "pink")
(set-face-attribute 'fringe nil :background (face-background 'default))

(when (eq system-type 'gnu/linux)
  (set-face-attribute 'default nil :family "DejaVu Sans Mono"
		      :height 110)
  (set-face-attribute 'variable-pitch nil :family "Noto Sans"))

(provide 'lina-faces)
