;; -*- lexical-binding: t; -*-
(defun delete-frame-or-kill-emacs ()
  (interactive)
  (condition-case e
      (delete-frame nil t)
    (error
     (save-buffers-kill-emacs))))

(provide 'delete-frame-or-kill-emacs)
