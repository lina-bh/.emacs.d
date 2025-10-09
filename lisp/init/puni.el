;; -*- lexical-binding: t; -*-
(use-package puni
  :ensure t
  :config
  (defun my-puni-c-w-dwim (&optional prefix)
    "if region is active delete whats in the region. otherwise, delete the
preceding sexp"
    (interactive "p")
    (if (use-region-p)
        (puni-kill-region)
      (backward-kill-sexp prefix)))
  (defun my-puni-kill-whole-line ()
    "delete the whole ass line the point is on"
    (interactive)
    (let ((kill-whole-line t))
      (move-beginning-of-line nil)
      (puni-kill-line)))
  :hook
  (puni-mode . electric-pair-local-mode)
  (prog-mode . puni-mode)
  :bind
  (:map puni-mode-map
        ("C-9" . puni-wrap-round)
        ("C-<backspace>" . puni-backward-kill-line)
        ("C-k" . my-puni-kill-whole-line)
        ("C-w" . my-puni-c-w-dwim)
        ("C-c DEL" . puni-force-delete)
        ("C-c r" . puni-raise)
        ("C-c ." . puni-slurp-forward)
        ("C-c s" . puni-splice))
  (:repeat-map my-puni-repeat-map
               ("." . puni-slurp-forward)))
