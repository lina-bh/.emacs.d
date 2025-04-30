(use-package finder
  :ensure nil
  :config
  (define-advice finder-commentary
      (:around (fun &rest args) no-shrink)
    (cl-letf
        (((symbol-function 'shrink-window-if-larger-than-buffer) #'ignore))
      (apply fun args))))
