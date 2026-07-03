;; -*- lexical-binding: t; -*-
(autoload 'eshell/cd "em-dirs" "Alias to extend the behavior of ‘cd’.

(fn &rest ARGS)" nil nil)
(autoload 'eshell-reset "esh-mode" "Output a prompt on a new line, aborting any current input.
If NO-HOOKS is non-nil, then ‘eshell-post-command-hook’ won’t be run.

(fn &optional NO-HOOKS)" nil nil)

;;;###autoload
(defun lina/eshell-in-buffer-directory ()
  "Create or switch to `eshell' in the `default-directory' of the selected buffer."
  (interactive)
  (let ((bufdir default-directory))
    (with-current-buffer (eshell)
      (unless (string= bufdir default-directory)
        (eshell/cd `(,bufdir))
        (eshell-reset)))))
