;; -*- lexical-binding: t; -*-
(eval-when-compile
  (require 'autoinsert))

(autoload 'comint-skip-input "comint" "Skip all pending input, from last stuff output by interpreter to point.
This means mark it as if it had been sent as input, without
sending it.  The command keys used to trigger the command that
called this function are inserted into the buffer." nil nil)
(autoload 'puni-mode "puni" "Enable keybindings for Puni commands.

This is a minor mode.  If called interactively, toggle the ‘Puni mode’
mode.  If the prefix argument is positive, enable the mode, and if it is
zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is ‘toggle’.  Enable the
mode if ARG is nil, omitted, or is a positive number.  Disable the mode
if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate the variable ‘puni-mode’.

The mode’s hook is called both when the mode is enabled and when it is
disabled.

(fn &optional ARG)" t nil)

(use-package elisp-mode
  :ensure nil
  :config
  (defun lina/elisp-hook ()
    (setq-local
     outline-regexp (rx (and ";;;" (0+ ";") blank))
     outline-imenu-generic-expression `(("Headings" ,(rx bol (regexp outline-regexp) (0+ nonl)) 0))
     imenu-generic-expression (append outline-imenu-generic-expression
                                      imenu-generic-expression)
     flymake-diagnostic-functions '(elisp-flymake-byte-compile t)
     elisp-flymake-byte-compile-load-path load-path
     trusted-content (append elisp-flymake-byte-compile-load-path
                             trusted-content))
    (when (fboundp 'dumb-jump-xref-activate)
      (setq-local xref-backend-functions '(dumb-jump-xref-activate
                                           elisp--xref-backend
                                           t)))
    (when (assq 'orderless completion-styles-alist)
      (setq-local completion-styles '(orderless)))
    (cond
     ((and (buffer-file-name)
           (file-in-directory-p (buffer-file-name) package-user-dir))
      (view-mode)
      (when (fboundp 'corfu-mode)
        (corfu-mode t)))
     ((string-match-p (rx bos "*Pp") (buffer-name))
      (view-mode))
     (t
      (let ((auto-insert-query nil)
            (auto-insert-alist
             `(("\\.el\\'"
                .
                ,(lambda ()
                   (setq-local lexical-binding t)
                   (add-file-local-variable-prop-line 'lexical-binding t)
                   (goto-char (point-max)))))))
        (auto-insert))))
    (show-paren-local-mode))
  :hook (emacs-lisp-mode-hook . lina/elisp-hook)
  :bind (:map emacs-lisp-mode-map
              ("C-c C-c" . elisp-eval-region-or-buffer)))

(use-package ielm
  :commands ielm-return
  :ensure nil
  :config
  (defun lina/ielm-interrupt ()
    (interactive)
    (comint-skip-input)
    (ielm-return))
  :bind (:map inferior-emacs-lisp-mode-map ("C-c C-c" . lina/ielm-interrupt)))

(use-package pp
  :functions (pp-display-expression@select-window
              pp-macroexpand-last-sexp@use-package-expand-minimally)
  :ensure nil
  :config
  (define-advice pp-display-expression (:after (_expression out-buffer-name &rest _) select-window)
    (let ((window (get-buffer-window out-buffer-name)))
      (when (window-live-p window)
        (select-window window))))
  (define-advice pp-macroexpand-last-sexp (:around (func &rest args) use-package-expand-minimally)
    (let ((use-package-expand-minimally t))
      (apply func args)))
  :bind (:map emacs-lisp-mode-map ("C-c C-p" . pp-macroexpand-last-sexp)))

(use-package aggressive-indent
  :ensure t
  :pin gnu
  :hook (lisp-data-mode-hook . aggressive-indent-mode))

(provide 'lina-elisp)
