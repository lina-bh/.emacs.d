;; -*- lexical-binding: t; -*-
(use-package window
  :ensure nil
  :custom
  (display-buffer-base-action '((display-buffer-reuse-window
                                 display-buffer-use-least-recent-window)))
  (switch-to-buffer-in-dedicated-window 'pop)
  (switch-to-buffer-obey-display-actions t)
  (split-window-preferred-direction 'horizontal)
  :config
  (defun split-window-right-and-select (&rest args)
    (interactive)
    (select-window (apply #'split-window-right args)))
  (defun split-window-below-and-select (&rest args)
    (interactive)
    (select-window (apply #'split-window-below args)))
  :bind
  ("C-x 2" . split-window-below-and-select)
  ("C-x 3" . split-window-right-and-select)
  ("C-x 5" . make-frame-command)
  ("C-x q" . quit-window)
  ("C-x o" . other-window)
  ("C-x 4" . other-window-prefix))

(setopt display-buffer-alist
        `(
          ((derived-mode . Info-mode)
           display-buffer-reuse-mode-window)
          ((derived-mode . magit-diff-mode)
           (display-buffer-reuse-mode-window)
           (mode . magit-log-mode))
          ("\\*Completions"
           (display-buffer-reuse-window
            display-buffer-at-bottom))
          ((derived-mode . calc-mode)
           display-buffer-at-bottom)
          ((or
            (category . man)
            (major-mode . Man-mode)
            ,(rx bos "*Man"))
           (display-buffer-reuse-mode-window)
           (mode . Man-mode))
          (,(rx bos "*Customize")
           display-buffer-reuse-mode-window)
          (,(rx bos "*Pp")
           (display-buffer-reuse-window
            display-buffer-below-selected)
           (dedicated . t))
          ((and (not ,(rx bos "*Async Shell Command*" eos))
                (or (category . comint)
                    (derived-mode . term-mode)
                    (derived-mode . comint-mode)
                    (derived-mode . ghostel-mode)
                    ,(rx bos "*" (or
                                  (and (* nonl) "REPL")
                                  "eshell"))))
           display-buffer-reuse-mode-window)
          ((or
            (category . warning)
            (derived-mode . flymake-diagnostics-buffer-mode)
            (derived-mode . help-mode)
            (derived-mode . compilation-mode)
            ,(rx bos "*" (or
                          "trace-output"
                          "eldoc"
                          "Warnings"
                          "Compile-Log")))
           display-buffer-in-side-window
           (window-height . 12)
           (slot . 0))))

(provide 'lina-window)
