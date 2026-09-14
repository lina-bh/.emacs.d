;;; lina-eglot.el --- lina-eglot  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Lina Bhaile <emacs-devel@linabee.uk>

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

(require 'eglot)

(setopt eglot-stay-out-of '(flymake eldoc)
        eglot-send-changes-idle-time 1
        eglot-ignored-server-capabilities '(:documentOnTypeFormattingProvider
                                            :documentHighlightProvider)
        eglot-server-programs
        `(((python-mode python-ts-mode) "ty" "server")
          (haskell-mode "haskell-language-server-wrapper" "--lsp")
          ((js-json-mode json-ts-mode)
           "npx"
           "--package=@t1ckbase/vscode-langservers-extracted"
           "vscode-json-language-server"
           "--stdio")
          ((c-mode c-ts-mode c++-mode c++-ts-mode objc-mode)
           . ,(eglot-alternatives
               '("ccls" "clangd")))
          (((js-mode :language-id "javascript")
            (js-ts-mode :language-id "javascript")
            (tsx-ts-mode :language-id "typescriptreact")
            (typescript-ts-mode :language-id "typescript")
            (typescript-mode :language-id "typescript"))
           .
           ("npx" "typescript-language-server" "--stdio"))))

(defun lina-eglot-hover-eldoc-function (cb &rest _ignored)
  (eglot-hover-eldoc-function
   (lambda (docstring &rest args)
     (when (and docstring
                (string-match (rx "```" (* nonl) "\n" (group (* nonl)))
                              docstring))
       (plist-put args :echo (match-string-no-properties 1 docstring)))
     (apply cb docstring args))))


(add-hook 'eglot-managed-mode-hook
          (defun lina-eglot-hook ()
            (eglot-inlay-hints-mode (if (derived-mode-p 'python-base-mode)
                                        -1
                                      t))
            (dolist (f (list #'eglot-signature-eldoc-function
                             #'lina-eglot-hover-eldoc-function
                             #'eglot-highlight-eldoc-function))
              (add-hook 'eldoc-documentation-functions f t t))
            (eldoc-mode t)
            (add-hook 'flymake-diagnostic-functions
                      #'eglot-flymake-backend nil t)
            (flymake-mode t)))

(bind-key "<f2>" #'eglot-rename eglot-mode-map)

(provide 'lina-eglot)
;;; lina-eglot.el ends here
