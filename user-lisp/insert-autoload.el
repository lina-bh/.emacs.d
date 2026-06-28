;; -*- lexical-binding: t; -*-

;;;###autoload
(defun insert-autoload (func)
  (interactive "aInsert autoload for function: ")
  (when (autoloadp func)
    (error "%S is not loaded"))
  (prin1 (append `(autoload ',func)
                 (list
                  (file-name-base (symbol-file func 'defun))
                  (substring-no-properties (documentation func))
                  (commandp func)
                  (and (macrop func) ''macro)))
         (current-buffer)))
