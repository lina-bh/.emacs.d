(defun derived-modes? (mode &optional interactive)
  "What fucking modes does this major mode inherit?"
  (interactive
   (list major-mode t))
  (cl-labels ((f (modes)
                (if-let* ((mode (car modes))
                          (parent (get mode 'derived-mode-parent)))
                    (f (cons parent modes))
                  modes)))
    (let ((parents (f (list mode))))
      (when interactive
        (message "%s" parents))
      parents)))
