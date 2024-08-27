;; -*- lexical-binding: t; -*-
(setopt help-window-select t)

(with-eval-after-load 'finder
  (define-advice finder-commentary (:around (fun &rest args) no-shrink)
    (cl-letf (((symbol-function 'shrink-window-if-larger-than-buffer) #'ignore))
      (apply fun args))))

(provide 'my-help)
