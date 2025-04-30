(use-package pp
  :ensure nil
  :config
  (define-advice pp-display-expression
      (:after (&rest args) readonly)
    (let ((out-buffer-name (cadr args)))
      (with-current-buffer out-buffer-name
        (view-mode)
        (flymake-mode -1))))
  :bind
  ("M-:" . pp-eval-expression)
  (:map emacs-lisp-mode-map
        ("C-c C-p" . pp-macroexpand-last-sexp)))
