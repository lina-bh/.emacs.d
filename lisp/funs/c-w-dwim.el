;; -*- lexical-binding: t; -*-
(defun c-w-dwim (&optional prefix)
  (interactive "p")
  (if (use-region-p)
      (kill-region (region-beginning)
                   (region-end))
    (backward-kill-word (or prefix 1))))

(provide 'c-w-dwim)
