;; -*- lexical-binding: t; -*-

(defun indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max))))

(defun indent-last-sexp ()
  (interactive)
  (let ((saved-point (point)))
    (save-excursion
      (backward-sexp)
      (indent-region (point) saved-point))))

(defun split-and-follow-vertically ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))

(defun split-and-follow-horizontally ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))

(defun comment-line-in-place ()
  (interactive)
  (save-excursion (comment-line 1)))

(provide 'lina-funs)
