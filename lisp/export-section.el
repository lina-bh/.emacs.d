;; -*- lexical-binding: t; -*-
(require 'org)

(autoload 'unidecode-sanitize "unidecode")

(defun org-latex-export-section-to-pdf ()
  (interactive)
  (if-let ((heading (nth 0 (org-get-outline-path t)))
           (position (org-find-exact-headline-in-buffer
                      heading
                      (current-buffer)
                      t))
           (outfile (concat (unidecode-sanitize heading) ".tex")))
      (save-excursion
        (goto-char position)
        (org-export-to-file
            (funcall-interactively #'org-latex-export-to-pdf)))))

(provide 'export-section)
