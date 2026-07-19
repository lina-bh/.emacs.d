;; -*- lexical-binding: t; -*-
(eval-when-compile
  (require 'cl-lib))

(use-package hungry-delete
  :ensure t)

(use-package smartparens
  :ensure t
  :functions
  (python-indent-dedent-line-backspace@sp-backward-delete-char-advice)
  :preface
  (defconst lina-sp-python-modes '(python-mode
                                   inferior-python-mode
                                   python-ts-mode))
  :custom
  ((sp-echo-match-when-invisible nil)
   (sp-escape-quotes-after-insert nil)
   (sp-highlight-pair-overlay nil))
  :autoload (sp-kill-region
             sp-backward-kill-sexp
             sp-local-pair
             sp-pair
             sp-point-in-string-or-comment
             sp-wrap-with-pair
             show-smartparens-mode
             smartparens-mode)
  :init
  (with-eval-after-load 'tex-mode
    (require 'smartparens-latex))
  (electric-pair-mode -1)
  :config
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
  (sp-local-pair sp-lisp-modes "'" nil :actions nil)
  (dolist (open '("{" "[" "("))
    (sp-pair open nil :unless '(sp-in-string-p)))
  (dolist (open '("{" "["))
    (sp-pair open nil :post-handlers '((lina/sp-open-newline-between-pairs
                                        "RET"))))
  (sp-local-pair '(c-ts-mode js-json-mode go-ts-mode) "(" nil
                 :post-handlers '((lina/sp-open-newline-between-pairs
                                   "RET")))
  (dolist (mode sp-lisp-modes)
    (unless (memq mode sp-clojure-modes)
      (sp-local-pair mode "`" "'"
                     :when '(sp-in-string-p
                             sp-in-comment-p)
                     :skip-match
                     (lambda (_ms _mb _me)
                       (not (sp-point-in-string-or-comment))))))

  (dolist (mode lina-sp-python-modes)
    (cl-pushnew (list mode 'regexp "") sp-sexp-suffix))

  (sp-with-modes lina-sp-python-modes
    (sp-local-pair "'" "'" :unless '(sp-in-comment-p sp-in-string-quotes-p)
                   :post-handlers '(:add sp-python-fix-tripple-quotes))
    (sp-local-pair "\"" "\"" :post-handlers '(:add sp-python-fix-tripple-quotes))
    (sp-local-pair "'''" "'''")
    (sp-local-pair "\\'" "\\'")
    (sp-local-pair "\"\"\"" "\"\"\""))

  (defun sp-python-fix-tripple-quotes (id action _context)
    "Properly rewrap tripple quote pairs.

When the user rewraps a tripple quote pair to the other pair
type (i.e. ''' to \") we check if the old pair was a
tripple-quote pair and if so add two pairs to beg/end of the
newly formed pair (which was a single-quote \"...\" pair)."
    (when (eq action 'rewrap-sexp)
      (let ((old (plist-get sp-handler-context :parent)))
        (when (or (and (equal old "'''") (equal id "\""))
                  (and (equal old "\"\"\"") (equal id "'")))
          (save-excursion
            (sp-get sp-last-wrapped-region
              (goto-char :end-in)
              (insert (make-string 2 (aref id 0)))
              (goto-char :beg)
              (insert (make-string 2 (aref id 0)))))))))

  (define-advice python-indent-dedent-line-backspace
      (:around (func &rest args) sp-backward-delete-char-advice)
    "Fix indent."
    (if smartparens-strict-mode
        (cl-letf (((symbol-function 'delete-backward-char)
                   (lambda (arg &optional _killp)
                     (sp-backward-delete-char arg))))
          (apply func args))
      (apply func args)))

  (defun lina/sp-mode-hook ()
    (electric-pair-local-mode -1)
    (show-smartparens-mode t)
    (when (and (fboundp 'hungry-delete-mode)
               (not (memq major-mode '(bash-ts-mode dockerfile-ts-mode))))
      (hungry-delete-mode)))
  (defun lina/maybe-turn-on-smartparens-mode ()
    (unless (memq major-mode '(bash-ts-mode dockerfile-ts-mode python-ts-mode))
      (smartparens-mode)))
  (defun lina-sp-wrap-round (&optional _arg)
    (interactive "P")
    (sp-wrap-with-pair "("))
  :hook ((smartparens-mode-hook . lina/sp-mode-hook)
         (prog-mode-hook . lina/maybe-turn-on-smartparens-mode)
         (lisp-data-mode-hook . smartparens-strict-mode))
  :bind
  ((:map smartparens-mode-map
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
         ("M-9" . lina-sp-wrap-round)
         ("M-r" . sp-raise-sexp)
         ("M-<up>" . sp-backward-up-sexp)
         ("M-<down>" . sp-down-sexp)
         ("M-<left>" . sp-backward-parallel-sexp)
         ("M-<right>" . sp-forward-parallel-sexp))
   (:map smartparens-strict-mode-map
         ("C-c DEL" . (lambda ()
                        (interactive)
                        (call-interactively #'backward-delete-char))))
   (:repeat-map smartparens-slurp-repeat-map
                ("." . sp-forward-slurp-sexp))
   (:repeat-map smartparens-splice-repeat-map
                ("s" . sp-splice-sexp))))
