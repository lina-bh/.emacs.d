;; -*- lexical-binding: t; -*-
(setq use-package-always-defer t)

(use-package package
  :custom
  (package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("nongnu" . "https://elpa.nongnu.org/nongnu/")
     ("melpa" . "https://melpa.org/packages/")
     ("gnu-devel" . "https://elpa.gnu.org/devel/")))
  (package-archive-priorities '(("gnu-devel" . -1))))

(load (setq custom-file (locate-user-emacs-file "custom.el")) t t t t)

(use-package exec-path-from-shell
  :custom
  (exec-path-from-shell-variables
   '("PATH" "MANPATH" "INFOPATH" "GOPATH" "JAVA_HOME"))
  :hook (after-init . exec-path-from-shell-initialize))

(add-to-list 'load-path (locate-user-emacs-file "lisp/"))
(require 'lina-core)
(load "lina-modes")
(load "lina-compl")
(require 'lina-check)
(load "lina-tools")
(load "lina-theme")
;; (load "lina-java")
(load "lina-org")
(load "lina-tex")
(load "lina-sexp")
(load "lina-js")
(load "lina-python")
(when (package-installed-p 'poly-R)
  (load "lina-poly"))
(pcase system-type
  ('windows-nt (load "lina-w32"))
  ('darwin (load "lina-macos")))

(use-package reformatter
  :config
  (define-advice reformatter--do-region (:around (fun &rest args)
                                                 quit-not-delete)
    (cl-letf (((symbol-function #'delete-windows-on)
               #'quit-windows-on))
      (apply fun args))))

(use-package window
  :custom
  (switch-to-buffer-obey-display-actions t)
  (switch-to-buffer-in-dedicated-window 'pop)
  (display-buffer-base-action '((display-buffer-reuse-window
                                 display-buffer-in-previous-window
                                 display-buffer-reuse-mode-window
                                 display-buffer-use-least-recent-window)))
  (display-buffer-alist
   (cl-labels ((derived-mode (mode)
                 (when (symbolp mode)
                   (setq mode (symbol-name mode)))
                 (cons 'derived-mode (intern (format "%s-mode" mode))))
               (derived-modes (&rest modes)
                 (let (xs)
                   (dolist (mode modes)
                     (push (derived-mode mode) xs))
                   xs)))
     `((,(rx bos "*Pp")
        display-buffer-below-selected)
       ((and (not (or ,@(derived-modes 'Info 'package-menu)
                      "COMMIT_EDITMSG"))
             (or ,@(derived-modes 'comint
                                  'eshell
                                  'special
                                  'term
                                  'flymake-project-diagnostics
                                  'flymake-diagnostics-buffer
                                  'apropos
                                  'compilation)
                 (category . comint)
                 ,(rx bos "*" (or "Finder"
                                  "Customize"
                                  "Embark"
                                  "Man"
                                  "TeX Help"))))
        display-buffer-in-side-window
        (window-height . ,(/ 1.0 3))))))
  :bind
  ("C-x =" . balance-windows)
  ("C-x q" . quit-window)
  ("s-w" . quit-window))

(defun delete-visited-file ()
  "Delete the file in the current buffer."
  (interactive)
  (let* ((buffer (current-buffer))
         (file-name (buffer-file-name buffer)))
    (if file-name
        (when (y-or-n-p (format "Delete %s?" file-name))
          (funcall-interactively #'delete-file file-name)
          (kill-buffer buffer))
      (message "Buffer not visiting any file"))))

(defun which ()
  "Show the path to a command."
  (interactive)
  (if-let* ((command (read-shell-command "Which command: "))
            (path (executable-find command)))
      (message "%s" path)
    (message "%s is not recognized as an internal or external command, operable\
 program or batch file." command)))

(defvar-local hide-cursor--original nil)

(define-minor-mode hide-cursor-mode
  "https://karthinks.com/software/more-less-emacs/"
  :global nil
  :lighter " HideCursor"
  (if hide-cursor-mode
      (setq-local hide-cursor--original cursor-type
                  cursor-type nil)
    (setq-local cursor-type (or hide-cursor--original t))))

(require 'server)
(unless (server-running-p)
  (server-start))
