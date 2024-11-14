;; -*- lexical-binding: t; -*-
(require 'vterm)
(autoload 'project-root "project")
(autoload 'project-current "project")
(autoload 'project-prefixed-buffer-name "project")

(defun project-vterm (&optional prefix)
  (interactive "P")
  (let ((default-directory (project-root (project-current t)))
        (vterm-buffer-name (project-prefixed-buffer-name "vterm")))
    (vterm prefix)))

(provide 'project-vterm)
