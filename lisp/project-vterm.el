;; -*- lexical-binding: t; -*-
(require 'vterm)
(autoload 'project-root "project")
(autoload 'project-current "project")
(autoload 'project-prefixed-buffer-name "project")

(defun project-vterm (&optional prefix)
  (interactive "P")
  (let ((buffer-directory (directory-file-name default-directory))
        (default-directory (project-root (project-current t)))
        (vterm-buffer-name (project-prefixed-buffer-name "vterm")))
    (vterm prefix)
    (when (vterm--at-prompt-p)
      (with-current-buffer vterm-buffer-name
        (vterm-insert "cd " buffer-directory)
        (vterm-send-return)))))

(provide 'project-vterm)
