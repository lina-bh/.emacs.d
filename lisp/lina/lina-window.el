;; -*- lexical-binding: t; -*-
(defun split-window-right-and-select (&rest args)
  (interactive)
  (select-window (apply #'split-window-right args)))

(defun split-window-below-and-select (&rest args)
  (interactive)
  (select-window (apply #'split-window-below args)))

(setopt display-buffer-base-action '((display-buffer-reuse-window
                                      display-buffer-use-least-recent-window))
        switch-to-buffer-in-dedicated-window 'pop
        switch-to-buffer-obey-display-actions t
        split-window-preferred-direction 'horizontal)

(setopt display-buffer-alist
        `(
          ((or (category . xref-jump)
               (category . comint)
               (derived-mode . Info-mode)
               "\\*ielm")
           .
           ,display-buffer-base-action)
          ("\\*Tetris\\*"
           display-buffer-full-frame)
          ((or
            "COMMIT_EDITMSG"
            (derived-mode . magit-mode))
           display-buffer-reuse-mode-window
           (mode . magit-mode))
          ("\\*Completions"
           (display-buffer-reuse-window
            display-buffer-at-bottom))
          ((derived-mode . calc-mode)
           display-buffer-at-bottom)
          ("\\*Customize"
           display-buffer-reuse-mode-window)
          ((and (not (major-mode . grep-mode))
                (or
                 (category . warning)
                 (derived-mode . flymake-diagnostics-buffer-mode)
                 (derived-mode . help-mode)
                 ,(rx "*" (or
                           "trace-output"
                           "eldoc"
                           "Warnings"
                           "Compile-Log"
                           "Checkdoc"
                           "Pp"
                           "compilation"
                           "claude"))))
           display-buffer-in-side-window
           (window-height . 16)
           (preserve-size . (nil . t))
           (slot . 0))))

(bind-keys
 ("C-x 1" . same-window-prefix)
 ("C-x 2" . split-window-below-and-select)
 ("C-x 3" . split-window-right-and-select)
 ("C-x q" . quit-window))
