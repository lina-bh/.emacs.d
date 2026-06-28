;; -*- lexical-binding: t; -*-
;;;###autoload
(defun autoload-cookie ()
  "Insert an autoload cookie above the current defun."
  (interactive)
  (save-excursion
    (beginning-of-defun)
    (insert ";;;###autoload\n")))
