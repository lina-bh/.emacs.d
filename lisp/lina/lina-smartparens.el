;; -*- lexical-binding: t; -*-
(when (featurep 'lina-puni)
  (error "load one or the other idiot"))

(use-package smartparens
  :ensure t
  :pin melpa-stable
  :init
  (require 'smartparens-config)
  :custom
  (sp-echo-match-when-invisible nil)
  (sp-escape-quotes-after-insert nil)
  (sp-highlight-pair-overlay nil)
  :config
  (defun lina/sp-mode-hook ()
    (show-paren-local-mode -1)
    (show-smartparens-mode t))
  (defun lina/sp-c-w-dwim ()
    "If region is active, call `sp-kill-region'. Otherwise, call `sp-backward-kill-sexp'."
    (interactive)
    (if (use-region-p)
        (sp-kill-region (region-beginning) (region-end))
      (sp-backward-kill-sexp current-prefix-arg)))
  (defun lina/sp-open-newline-between-pairs (&rest _args)
    (newline)
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode))
  (dolist (it '("{" "[" "("))
    (sp-local-pair
     '(c-ts-mode) it nil
     :post-handlers '((lina/sp-open-newline-between-pairs
                       "RET"))))
  :hook
  (smartparens-mode-hook . lina/sp-mode-hook)
  (prog-mode-hook . smartparens-mode)
  ((lisp-data-mode-hook inferior-emacs-lisp-mode-hook racket-mode-hook)
   . smartparens-strict-mode)
  :bind
  (:map smartparens-mode-map
        ("C-t" . sp-transpose-sexp)
        ("C-w" . lina/sp-c-w-dwim)
        ("C-k" . sp-kill-whole-line)
        ("C-c ." . sp-forward-slurp-sexp)
        ("C-c s" . sp-splice-sexp)
        ("C-c r" . sp-raise-sexp)
        ("M-<left>" . sp-backward-sexp)
        ("M-<right>" . sp-forward-sexp))
  (:repeat-map smartparens-mode-repeat-map
               ("." . sp-forward-slurp-sexp)))

(use-package hungry-delete
  :ensure t
  :hook (prog-mode-hook . hungry-delete-mode))

(provide 'lina-smartparens)
