;; -*- lexical-binding: t; -*-
(defvar find-dominating-file-history nil)

(defun find-dominating-file (name)
  "Starting at the current visited file, walk backwards up the directory
hierarchy for a file called NAME and find that file."
  (interactive
   (list
    (read-from-minibuffer "Find dominating file name: " nil nil nil
                          'find-dominating-file-history)))
  (if-let ((dir (locate-dominating-file (buffer-file-name) name)))
      (find-file (expand-file-name name dir))
    (user-error "No such file found")))

(provide 'find-dominating-file)
