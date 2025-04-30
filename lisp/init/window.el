(use-package window
  :ensure nil
  :preface
  (defun match-derived-modes (&rest modes)
    (let (xs)
      (dolist (mode modes)
        (push (cons 'derived-mode (intern (format "%s-mode" mode))) xs))
      xs))
  (defconst tray-buffer-criteria
    `(and (not (or ,@(match-derived-modes 'Info 'package-menu 'eww)
                   ,(rx bos "magit-diff")
                   "Shell Command"))
          (or ,@(match-derived-modes 'comint
                                     'special
                                     'term
                                     'flymake-project-diagnostics
                                     'flymake-diagnostics-buffer
                                     'apropos
                                     'compilation)
              (category . comint)
              ,(rx bos "*" (or "Finder"
                               ;; "Embark"
                               "TeX Help"
                               "Agenda Commands"
                               "Org Export Dispatcher"
                               "Org PDF LaTeX Output"
                               "devcontainer"
                               "Nix-REPL"))
              ,(rx (or "shell" "vterm" "eshell") "*")
              "COMMIT_EDITMSG")))
  :custom
  (display-buffer-base-action '((display-buffer-reuse-window
                                 display-buffer-in-previous-window
                                 display-buffer-reuse-mode-window
                                 display-buffer-use-least-recent-window)))
  (switch-to-buffer-obey-display-actions t)
  (switch-to-buffer-in-dedicated-window 'pop)
  (display-buffer-alist
   `((,(rx bos "*Pp")
      (display-buffer-in-previous-window
       display-buffer-below-selected))
     (,(rx bos "*Customiz")
      (display-buffer-reuse-mode-window
       display-buffer-pop-up-window))
     (,(rx bos "*" (or "Man"
                       "info"))
      (display-buffer-reuse-mode-window
       display-buffer-same-window))
     (,tray-buffer-criteria
      display-buffer-in-side-window
      (window-height . ,(/ 1.0 3)))))
  :bind
  (("C-x =" . balance-windows)
   ("C-x q" . quit-window)
   ("C-x [" . previous-buffer)))
