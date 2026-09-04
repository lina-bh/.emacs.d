;; -*- lexical-binding: t; -*-
(eval-when-compile
  (require 'autoinsert)
  (require 'cl-lib))

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

(defconst lina-elisp-auto-insert '(nil
                                   ";;; "
                                   (file-name-nondirectory (buffer-file-name))
                                   " --- "
                                   (file-name-base (buffer-file-name))
                                   "  -*- lexical-binding: t; -*-" '(setq lexical-binding t)
                                   "

;; Copyright (C) " (format-time-string "%Y") " Lina Bhaile <emacs-devel@linabee.uk>

;; Author: Lina Bhaile <emacs-devel@linabee.uk>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

" _ "

(provide '"
                                   (file-name-base (buffer-file-name))
                                   ")
;;; " (file-name-nondirectory (buffer-file-name)) " ends here\n"))

(use-package elisp-mode
  :functions (lina-display-log-after-interactive-compile)
  :ensure nil
  :config
  (setf (alist-get '("\\.el\\'" . "Emacs Lisp header")
                   auto-insert-alist
                   nil
                   nil
                   #'equal)
        lina-elisp-auto-insert)
  (defun lina-display-log-after-interactive-compile (&rest _ignore)
    (display-buffer byte-compile-log-buffer))
  (advice-add #'elisp-byte-compile-buffer :after
              #'lina-display-log-after-interactive-compile)
  (defun lina-elisp-hook ()
    (setq-local
     outline-regexp (rx (and ";;;" (0+ ";") blank))
     outline-imenu-generic-expression
     `(("Headings" ,(rx bol (regexp outline-regexp) (0+ nonl)) 0))
     imenu-generic-expression (append outline-imenu-generic-expression
                                      imenu-generic-expression)
     flymake-diagnostic-functions '(elisp-flymake-byte-compile t)
     elisp-flymake-byte-compile-load-path load-path)
    (when (fboundp 'dumb-jump-xref-activate)
      (setq-local xref-backend-functions '(elisp--xref-backend
                                           dumb-jump-xref-activate
                                           t)))
    (when (assq 'orderless completion-styles-alist)
      (setq-local completion-styles '(orderless)))
    (add-hook 'before-save-hook #'check-parens nil t)
    (cond
     ;; ((and (buffer-file-name)
     ;;       (file-in-directory-p (buffer-file-name) package-user-dir))
     ;;  (view-mode)
     ;;  (when (fboundp 'corfu-mode)
     ;;    (corfu-mode t)))
     ((or (eq major-mode 'elisp-byte-code-mode)
          (string-match-p (rx bos "*Pp") (buffer-name)))
      (view-mode))
     (t
      (let ((auto-insert-query nil))
        (auto-insert))
      (flymake-mode)))
    (show-paren-local-mode))
  (defun lina-lisp-data-hook ()
    "Hook for `lisp-data-mode' and descendant modes i.e `emacs-lisp-mode'."
    (setq-local completion-at-point-functions
                `(,@(and (fboundp 'cape-capf-super)
                         (fboundp 'cape-elisp-symbol)
                         (list (cape-capf-super #'elisp-completion-at-point
                                                #'cape-elisp-symbol)))
                  elisp-completion-at-point
                  ,@(default-value 'completion-at-point-functions)
                  t)))
  :hook ((lisp-data-mode-hook . lina-lisp-data-hook)
         (emacs-lisp-mode-hook . lina-elisp-hook))
  :bind
  (:map emacs-lisp-mode-map
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
  :bind (:map lisp-mode-shared-map ("C-c C-p" . pp-macroexpand-last-sexp)))

(use-package ert
  :ensure nil
  :bind (:map emacs-lisp-mode-map
              ("C-c C-t" . ert-run-tests-interactively)))

(use-package aggressive-indent
  :ensure t
  :pin gnu
  :custom
  ((aggressive-indent-dont-indent-if
    `((and (memq major-mode '(c-mode c-ts-mode))
           (null (string-match-p ,(rx
                                   (or
                                    (any ";{}")
                                    (seq word-boundary
                                         (or "if" "for" "while")
                                         word-boundary))
                                   )
                                 (thing-at-point 'line)))))))
  :hook
  ((c-mode-hook lisp-data-mode-hook) . aggressive-indent-mode)
  :delight " aggro")

(provide 'lina-elisp)
