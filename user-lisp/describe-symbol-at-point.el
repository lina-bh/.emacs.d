;; -*- lexical-binding: t; -*-

;;;###autoload
(defun describe-symbol-at-point ()
  "Call `describe-symbol' on the symbol at point."
  (interactive)
  (describe-symbol (intern (thing-at-point 'symbol t))))
