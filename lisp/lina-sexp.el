;; -*- lexical-binding: t; -*-
(use-package elisp-mode
  :init
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
      (auto-insert)))
  :hook (emacs-lisp-mode . my-elisp-mode-hook)
  :bind (:map emacs-lisp-mode-map
              (";" . comment-dwim)
              ("C-c C-p" . pp-macroexpand-last-sexp))
  :mode ("\\.dir-locals\\(?:-2\\)?\\.el\\'" . emacs-lisp-mode))

(use-package pp
  :config
  (define-advice pp-display-expression (:after (&rest args) readonly)
    (let ((out-buffer-name (cadr args)))
      (with-current-buffer out-buffer-name
        (view-mode)
        (flymake-mode -1))))
  :bind
  ([remap eval-expression] . pp-eval-expression))

(use-package ielm
  :commands ielm-C-c
  :config
  (defun ielm-C-c ()
    (interactive)
    (comint-skip-input)
    (ielm-return))
  :bind (:map ielm-map ("C-c C-c" . ielm-C-c)))

(use-package puni
  :config
  (defvar-keymap my-puni-repeat-map
    :repeat t
    "." #'puni-slurp-forward
    "r" #'puni-raise)
  (defun my-puni-c-w-dwim ()
    (interactive)
    (if (use-region-p)
        (puni-kill-region)
      (backward-kill-sexp)))
  (defun my-puni-kill-whole-line ()
    (interactive)
    (let ((kill-whole-line t))
      (move-beginning-of-line nil)
      (puni-kill-line)))
  (put #'puni-slurp-forward 'repeat-map 'my-puni-repeat-map)
  (put #'puni-raise 'repeat-map 'my-puni-repeat-map)
  :hook (puni-mode . electric-pair-local-mode)
  :hook (prog-mode . puni-mode)
  :bind (:map puni-mode-map
              ("C-c r" . puni-raise)
              ("C-c ." . puni-slurp-forward)
              ("C-c s" . puni-splice)
              ("C-9" . puni-wrap-round)
              ("C-<backspace>" . puni-backward-kill-line)
              ("C-k" . my-puni-kill-whole-line)
              ("C-w" . my-puni-c-w-dwim)))

(use-package aggressive-indent
  :hook (emacs-lisp-mode . aggressive-indent-mode))
