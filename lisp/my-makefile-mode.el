;; -*- lexical-binding: t; -*-
(defun indent-makefile ()
  (interactive)
  (let (in-rule)
    (save-excursion
      (move-beginning-of-line 0)
      (when (and
             (not (bobp))
             (seq-some #'looking-at
                       '("\t" ".+:.*$")))
        (setq in-rule t)))
    (if in-rule
        (insert "\t")
      'noindent)))

(add-hook 'makefile-mode-hook
          (defun my-make-hook ()
            (setq-local indent-line-function #'indent-makefile
                        tab-always-indent t
                        whitespace-style '(face indentation tab-mark))
            (indent-tabs-mode)
            (whitespace-mode)))

(bind-keys :map emacs-lisp-mode-map
           (";" . comment-dwim)
           ("C-c C-p" . pp-macroexpand-last-sexp))

(provide 'my-makefile-mode)
