;; -*- lexical-binding: t; -*-
(when (eq system-type 'windows-nt)
  ;; stolen from doom-emacs/core/core.el
  (setq-default w32-enable-synthesized-fonts t
		w32-get-true-file-attributes nil
		w32-pipe-read-delay 0
		w32-pipe-buffer-size 65536))
(cd (getenv "USERPROFILE"))
(set-face-attribute 'default nil
		    :family "Consolas"
		    :height 110)
(set-face-attribute 'variable-pitch nil
		    :family "Calibri"
		    :height 115)

;; last resort for flyspell: use aspell inside WSL
(eval-after-load 'ispell
  (setq ispell-program-name (expand-file-name
			     "aspell.cmd"
			     user-emacs-directory)))
