(use-package make-mode
  :ensure nil
  :commands indent-makefile
  :config
  (defun indent-makefile ()
    (interactive)
    (let (in-rule)
      (save-excursion
        (move-beginning-of-line 0)
        (setq in-rule (and
                       (not (bobp))
                       (seq-some #'looking-at '("\t"))))) ;".+:.*$"
      (if in-rule
          (insert "\t")
        'noindent)))
  (defun my-makefile-hook ()
    (setq-local whitespace-style '(tab-mark))
    (whitespace-mode))
  (setq-mode-local makefile-mode
                   indent-tabs-mode t
                   indent-line-function #'indent-makefile
                   tab-always-indent t)
  :hook (makefile-mode . my-makefile-hook))
