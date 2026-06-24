;; -*- lexical-binding: t; -*-
(when (featurep 'lina-smartparens)
  (error "load one or the other idiot"))

(use-package puni
  :ensure t
  :config
  (defun lina/puni-c-w-dwim ()
    (interactive)
    (call-interactively (if (use-region-p)
                            #'puni-kill-active-region
                          #'backward-kill-sexp)))
  (defun lina/puni-kill-whole-line ()
    (interactive)
    (let ((kill-whole-line t))
      (move-beginning-of-line nil)
      (puni-kill-line)))
  (defun lina/puni-hungry-backward-delete-char ()
    (interactive)
    (if (or current-prefix-arg
            (use-region-p)
            (not (looking-back (rx line-start (+ blank)))))
        (call-interactively #'puni-backward-delete-char)
      (puni-delete-region (1- (line-beginning-position)) (point))))
  :hook
  (puni-mode-hook . electric-pair-local-mode)
  (prog-mode-hook . puni-mode)
  :bind
  (:map puni-mode-map
        ([remap puni-backward-delete-char] . lina/puni-hungry-backward-delete-char)
        ("C-k" . lina/puni-kill-whole-line)
        ("C-t" . puni-transpose)
        ("C-w" . lina/puni-c-w-dwim)
        ("C-c r" . puni-raise)
        ("C-c ." . puni-slurp-forward)
        ("C-c s" . puni-splice)
        ("M-<up>" . puni-backward-sexp-or-up-list)
        ("M-<left>" . puni-backward-sexp)
        ("M-<right>" . puni-forward-sexp))
  (:repeat-map lina/puni-repeat-map
               ("." . puni-slurp-forward)))

(provide 'lina-puni)
