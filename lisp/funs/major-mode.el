;; -*- lexical-binding: t; -*-
(defun major-mode? ()
  "What fucking mode is this?"
  (interactive)
  (message "%s" major-mode))

(provide 'major-mode)
