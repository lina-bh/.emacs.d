;; -*- lexical-binding: t; -*-
(add-hook 'emacs-lisp-mode-hook
          (defun my-elisp-mode-hook ()
            (require 'autoinsert)
            (require 'seq)
            (add-hook 'after-save-hook #'check-parens nil 'local)
            (remove-hook 'flymake-diagnostic-functions #'elisp-flymake-checkdoc t)
            (let ((buffer-file-name (buffer-file-name)))
              (and
               buffer-file-name
               (not (file-in-directory-p buffer-file-name
                                         (locate-user-emacs-file "lisp/")))
               (seq-some (apply-partially #'file-in-directory-p buffer-file-name)
                         load-path)
               (view-mode)))
            (let ((auto-insert-query nil))
              (auto-insert))))

(add-to-list 'auto-mode-alist
             '("\\.dir-locals\\(?:-2\\)?\\.el\\'" . emacs-lisp-mode))

(define-advice pp-display-expression (:after (&rest args) readonly)
  (let ((out-buffer-name (cadr args)))
    (with-current-buffer out-buffer-name
      (view-mode)
      (flymake-mode -1))))

(add-hook 'emacs-lisp-mode-hook #'flymake-mode)

(provide 'my-elisp-mode)
