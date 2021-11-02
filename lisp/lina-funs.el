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

(defun restart-emacs ()
  (interactive)
  (let* ((invocation-path
	  (expand-file-name invocation-name invocation-directory))
	 (kill-emacs-hook
	  (append kill-emacs-hook
		  (list (lambda ()
			  (call-process invocation-path nil 0 nil))))))
    (save-buffers-kill-emacs)))

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
