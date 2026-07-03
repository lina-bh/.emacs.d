;; -*- lexical-binding: t; -*-
(unless (package-installed-p 'smartparens)
  (package-install 'smartparens))
(require 'smartparens)

(with-eval-after-load 'tex-mode
  (require 'smartparens-latex))

(setopt sp-echo-match-when-invisible nil
        sp-escape-quotes-after-insert nil
        sp-highlight-pair-overlay nil)

(defun lina/sp-c-w-dwim ()
  "Call `sp-kill-region' if region is active, `sp-backward-kill-sexp'."
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
   '(c-ts-mode js-json-mode) it nil
   :post-handlers '((lina/sp-open-newline-between-pairs
                     "RET"))))

(defun sp-lisp-invalid-hyperlink-p (_id action _context)
  "Test if there is an invalid hyperlink in a Lisp docstring.
ID, ACTION, CONTEXT."
  (when (eq action 'navigate)
    ;; Ignore errors due to us being at the start or end of the
    ;; buffer.
    (ignore-errors
      (or (and (looking-at "\\sw\\|\\s_")
               (save-excursion
                 (backward-char 2)
                 (looking-at "\\sw\\|\\s_")))
          (and (save-excursion
                 (backward-char 1)
                 (looking-at "\\sw\\|\\s_"))
               (save-excursion
                 (forward-char 1)
                 (looking-at "\\sw\\|\\s_")))))))

(sp-pair "(" nil :unless '(sp-in-string-p))
(sp-local-pair sp-lisp-modes "'" nil :actions nil)

(sp-local-pair (seq-difference sp-lisp-modes sp-clojure-modes)
               "`" "'"
               :when '(sp-in-string-p
                       sp-in-comment-p)
               :unless '(sp-lisp-invalid-hyperlink-p)
               :skip-match
               (lambda (ms _mb _me)
                 (cond
                  ((equal ms "'")
                   (or (sp-lisp-invalid-hyperlink-p "`" 'navigate '_)
                       (not (sp-point-in-string-or-comment))))
                  (t (not (sp-point-in-string-or-comment))))))

(defun lina/sp-mode-hook ()
  (electric-pair-local-mode -1)
  (show-smartparens-mode t))
(add-hook 'smartparens-mode-hook #'lina/sp-mode-hook)

(add-hook 'lisp-data-mode-hook #'smartparens-strict-mode)
(add-hook 'tex-mode-hook #'smartparens-mode)

(bind-keys :map smartparens-mode-map
           ("DEL" . sp-backward-delete-char)
           ("<delete>" . sp-delete-char)
           ("C-c DEL" . backward-delete-char)
           ("C-t" . sp-transpose-sexp)
           ("C-w" . lina/sp-c-w-dwim)
           ("C-k" . sp-kill-whole-line)
           ("C-c ." . sp-forward-slurp-sexp)
           ("C-c ," . sp-forward-barf-sexp)
           ("C-c s" . sp-splice-sexp)
           ("C-c r" . sp-raise-sexp)
           ("M-r" . sp-raise-sexp)
           ("M-<up>" . sp-backward-up-sexp)
           ("M-<down>" . sp-down-sexp)
           ("M-<left>" . sp-backward-parallel-sexp)
           ("M-<right>" . sp-forward-parallel-sexp)
           :repeat-map smartparens-mode-repeat-map
           ("." . sp-forward-slurp-sexp))

(electric-pair-mode -1)
