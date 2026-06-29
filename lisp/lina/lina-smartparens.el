;; -*- lexical-binding: t; -*-
(use-package smartparens
  :ensure t
  :pin melpa-stable
  :custom
  (sp-echo-match-when-invisible nil)
  (sp-escape-quotes-after-insert nil)
  (sp-highlight-pair-overlay nil)
  :config
  (defun lina/sp-mode-hook ()
    (electric-pair-local-mode -1)
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
  (sp-pair "(" nil :unless '(sp-in-string-p))
  (dolist (it '("{" "[" "("))
    (sp-local-pair
     '(c-ts-mode js-json-mode) it nil
     :post-handlers '((lina/sp-open-newline-between-pairs
                       "RET"))))
  (sp-with-modes sp-lisp-modes
    (sp-local-pair "'" nil :actions nil))
  (require 'smartparens-latex)
  :hook
  (smartparens-mode-hook . lina/sp-mode-hook)
  :bind
  (:map smartparens-mode-map
        ("DEL" . sp-backward-delete-char)
        ("<delete>" . sp-delete-char)
        ("C-c DEL" . backward-delete-char)
        ("C-t" . sp-transpose-sexp)
        ("C-w" . lina/sp-c-w-dwim)
        ("C-k" . sp-kill-whole-line)
        ("C-c ." . sp-forward-slurp-sexp)
        ("C-c ," . sp-forward-barf-sexp)
        ("M-s" . sp-splice-sexp)
        ("M-r" . sp-raise-sexp)
        ("M-<up>" . sp-backward-up-sexp)
        ("M-<down>" . sp-down-sexp)
        ("M-<left>" . sp-backward-parallel-sexp)
        ("M-<right>" . sp-forward-parallel-sexp))
  (:repeat-map smartparens-mode-repeat-map
               ("." . sp-forward-slurp-sexp)))

(provide 'lina-smartparens)
