;; -*- lexical-binding: t; -*-
(autoload 'whitespace-cleanup-region "whitespace")

(defvar-local format-mode-command #'indent-region-or-buffer)

(defun indent-region-or-buffer (beg end)
  "Call `indent-region' on either the selected region or the whole buffer."
  (interactive "r")
  (unless (use-region-p)
    (setq beg (point-min)
          end (point-max)))
  (save-excursion
    (indent-region beg end)
    (whitespace-cleanup-region beg end)))

(defun format-mode ()
  (interactive)
  (call-interactively format-mode-command))

(provide 'format-mode)
