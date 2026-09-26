;; -*- lexical-binding: t; -*-
(require 'package)

(setopt org-modules nil
        org-export-backends '(html md)
        org-link-descriptive nil)

(add-hook 'org-mode-hook
          (defun lina-org-hook ()
            (variable-pitch-mode)))
