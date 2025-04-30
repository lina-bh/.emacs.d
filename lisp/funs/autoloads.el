;; -*- lexical-binding: t; -*-
(require 'major-mode (expand-file-name "../major-mode.el" load-file-name))
(require 'c-w-dwim (expand-file-name "../c-w-dwim.el" load-file-name))
(require 'delete-frame-or-kill-emacs (expand-file-name "../delete-frame-or-kill-emacs.el" load-file-name))
(require 'split-and-follow (expand-file-name "../split-and-follow.el" load-file-name))

(autoload 'delete-visited-file (expand-file-name "../delete-visited-file.el" load-file-name) nil t)
(autoload 'derived-modes? (expand-file-name "../derived-modes.el" load-file-name) nil t)
(autoload 'major-mode? (expand-file-name "../major-mode.el" load-file-name) nil t)
(autoload 'which (expand-file-name "../which.el" load-file-name) nil t)
(autoload 'indent-region-or-buffer (expand-file-name "../indent-region-or-buffer.el" load-file-name) nil t)
