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
          ((category . xref-jump)
           (display-buffer-reuse-window
            display-buffer-same-window))
          ((or (category . comint)
               (derived-mode . Info-mode)
               "\\*ielm"
               "\\*eww"
               ;; "\\*gud"
               )
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
          ((or (derived-mode . help-mode)
               "\\*sly-description")
           display-buffer-in-side-window
           (window-height . ,(/ 1.0 3))
           (preserve-size . (nil . t))
           (no-delete-other-windows . t)
           (slot . 1))
          ((and (not (major-mode . grep-mode))
                (or
                 (category . warning)
                 (derived-mode . flymake-diagnostics-buffer-mode)
                 (derived-mode . flymake-project-diagnostics-mode)
                 (derived-mode . gdb-inferior-io-mode)
                 ,@(let ((regexps (list)))
                     (dolist (prefix '("trace-output"
                                       "eldoc"
                                       "Warnings"
                                       "Compile-Log"
                                       "Checkdoc"
                                       "Pp"
                                       "compilation"
                                       "claude"
                                       "Backtrace"
                                       "sly-macroexpansion")
                                     regexps)
                       (push (concat "\\*" prefix) regexps)))))
           display-buffer-in-side-window
           (window-height . ,(/ 1.0 3))
           (preserve-size . (nil . t))
           (slot . 0)
           (window-parameters
            (no-delete-other-windows . t)))))

(bind-keys
 ("C-x 1" . same-window-prefix)
 ("C-x 2" . split-window-below-and-select)
 ("C-x 3" . split-window-right-and-select)
 ("C-x q" . quit-window))
