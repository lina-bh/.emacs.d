;; -*- lexical-binding: t; -*-
(autoload #'browse-url-with-browser-kind "browse-url" nil t)
(autoload #'browse-url-file-url "browse-url")

(defvar-local browse-file-last-path nil
  "Last path given to `browse-file'. Can be set locally to set default path.")

(defun browse-file (path)
  "Call external browser to open PATH.

Interactively when `browse-file-last-path' is nil or command is prefixed,
prompt for PATH. Sets `browse-file-last-path' to last successful browsed
file."
  (interactive
   `(,(if (or (null browse-file-last-path) current-prefix-arg)
          (read-file-name "Open file: " nil nil t)
        browse-file-last-path)))
  (let ((proc (browse-url-with-browser-kind
               'external (browse-url-file-url (expand-file-name path)))))
    (when (= (process-exit-status proc) 0)
      (setq-local browse-file-last-path path))))

(provide 'browse-file)
