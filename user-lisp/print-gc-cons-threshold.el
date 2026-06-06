;; -*- lexical-binding: t; -*-

;;;###autoload
(defun print-gc-cons-threshold (&optional interactive)
  "Human-readable value of `gc-cons-threshold'."
  (interactive (list t))
  (let ((human (file-size-human-readable gc-cons-threshold)))
    (when interactive
      (message "%s" human))
    human))
