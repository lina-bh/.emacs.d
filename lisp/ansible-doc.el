;; -*- lexical-binding: t; -*-
(eval-and-compile
  (require 'cl-lib))

(defun ansible-doc--plugins ()
  (interactive)
  (let ((ansible-doc (executable-find "ansible-doc")))
    (with-temp-buffer
      (call-process ansible-doc nil t nil "-jl")
      (goto-char (point-min))
      (let ((plugins (json-parse-buffer)))
        plugins))))

(defun ansible-doc (plugin)
  (interactive
   "MPlugin: \n")
  (let* (buffer
         (sentinel (lambda (proc status)
                     (with-current-buffer buffer
                       (cond
                         ((eq (char-after 1) ?>)
                          (goto-char (point-min))
                          (ansi-color-apply-on-region (point-min)
                                                      (point-max))
                          (run-mode-hooks)
                          (setq-local buffer-read-only t)
                          (display-buffer buffer))
                         (t
                          (unwind-protect
                               (user-error
                                "%s" (buffer-substring-no-properties
                                      (point-min)
                                      (- (point-max) 1)))
                            (kill-buffer buffer)))))))
         (buffer-name (format "*ansible-doc %s*" plugin)))
    (if (setq buffer (get-buffer buffer-name))
        (display-buffer buffer)
      (progn
        (setq buffer (get-buffer-create buffer-name))
        (make-process
         :name "ansible-doc"
         :buffer buffer
         :command (list "ansible-doc" plugin)
         :noquery t
         :connection-type 'pipe
         :sentinel sentinel))))
  plugin)

(provide 'ansible-doc)
