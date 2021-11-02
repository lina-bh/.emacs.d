;; -*- lexical-binding: t; -*-

(add-hook 'prog-mode-hook (defun lina/prog-mode-hook ()
			    (column-number-mode t)
			    (display-line-numbers-mode t)
			    (electric-pair-local-mode t)
			    (show-paren-local-mode t)
			    (setq-local indicate-empty-lines t
					show-trailing-whitespace t)))

(add-hook 'text-mode-hook (defun lina/text-mode-hook ()
			    (visual-line-mode t)
			    (variable-pitch-mode t)
			    (setq-local cursor-type 'bar)))

(add-hook 'emacs-lisp-mode-hook
	  (defun lina/elisp-hook ()
	    (set-buffer-file-coding-system 'unix)
	    (display-fill-column-indicator-mode t)))

(provide 'lina-hooks)
