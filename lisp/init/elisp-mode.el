;; -*- lexical-binding: t; -*-
(use-package elisp-mode
  :ensure nil
  :functions elisp-enable-lexical-binding my-elisp-view-hook
  :config
  (defun my-elisp-view-hook ()
    (and
     buffer-file-name
     (not (file-in-directory-p buffer-file-name
                               (locate-user-emacs-file "lisp/")))
     (seq-some (lambda (path)
                 (file-in-directory-p buffer-file-name path))
               load-path)
     (view-mode)))
  (defun my-elisp-mode-hook ()
    (setq-local elisp-flymake-byte-compile-load-path load-path)
    (add-hook 'after-save-hook #'check-parens nil 'local)
    (remove-hook 'flymake-diagnostic-functions #'elisp-flymake-checkdoc t)
    (my-elisp-view-hook)
    ;; (unless buffer-read-only
    ;;   (let ((auto-insert-query nil))
    ;;     (auto-insert)))
    )
  (define-advice elisp-enable-lexical-binding
      (:around (fun &rest _) no-ask)
    (save-excursion
      (funcall fun nil)))
  :hook (emacs-lisp-mode . my-elisp-mode-hook)
  :bind (:map emacs-lisp-mode-map
              (";" . comment-dwim)
              ("C-c C-c" . elisp-eval-region-or-buffer))
  :mode ("\\.dir-locals\\(?:-2\\)?\\.el\\'" . emacs-lisp-mode))

(use-package erefactor
  :ensure t
  :bind (:map emacs-lisp-mode-map
              ("<f2>" . erefactor-rename-symbol-in-buffer)))
