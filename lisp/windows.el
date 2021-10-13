(cd (getenv "USERPROFILE"))
(set-face-attribute 'default nil
		    :family "Consolas"
		    :height 110)
(set-face-attribute 'variable-pitch nil
		    :family "Calibri"
		    :height 115)

;; stolen from doom-emacs/core/core.el
(when (boundp 'w32-get-true-file-attributes)
  (setq w32-get-true-file-attributes nil
	w32-pipe-read-delay 0
	w32-pipe-buffer-size 65536))

