;; -*- lexical-binding: t; -*-
(autoload 'eglot-rename "eglot" nil t)
(autoload 'eglot-managed-p "eglot" nil t)

(setopt eglot-ignored-server-capabilities '(:inlayHintProvider)
        eglot-report-progress nil
        eglot-events-buffer-config '(:size nil :format full))

(with-eval-after-load 'eglot
  (setf (alist-get 'web-mode eglot-server-programs)
        (alist-get 'html-mode eglot-server-programs))
  (bind-keys :map 'eglot-mode-map
           ("<f2>" . eglot-rename)))
(add-hook 'eglot-managed-mode-hook
          (defun my-eglot-hook ()
            (setq-local eldoc-echo-area-use-multiline-p
                        (eglot-managed-p))))

(provide 'my-eglot)
