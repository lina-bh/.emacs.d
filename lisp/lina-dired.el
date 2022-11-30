;; -*- lexical-binding: t; -*-

(use-package dired
  :hook (dired-mode . dired-hide-details-mode))

(use-package dired-single
  :ensure t
  :after (dired)
  :bind (:map dired-mode-map
	      ([remap dired-find-file]
	       .
	       dired-single-buffer)
	      ([remap dired-mouse-find-file-other-window]
	       .
	       dired-single-buffer-mouse)
	      ([remap dired-up-directory]
	       .
	       dired-single-up-directory)))

(provide 'lina-dired)
