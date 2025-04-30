(use-package cape
  :ensure t
  :config
  (defun my-elisp-cape-hook ()
    (add-hook 'completion-at-point-functions
              #'cape-elisp-symbol -1 t))
  (defun my-cape-dabbrev-hook ()
    (add-hook 'completion-at-point-functions
              #'cape-dabbrev -1 t))
  :hook (((emacs-lisp-mode inferior-emacs-lisp-mode)
          .
          my-elisp-cape-hook)
         ((hcl-mode yaml-mode) . my-cape-dabbrev-hook))
  ;; :bind (:map org-mode-map
  ;;             ("C-c ?" . cape-tex))
  )
