;; -*- lexical-binding: t; -*-
(require 'eglot)
(setopt eglot-stay-out-of '(flymake eldoc)
        eglot-send-changes-idle-time 1
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
(defun lina/eglot-hook ()
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
  (flymake-mode t))
(add-hook 'eglot-managed-mode-hook #'lina/eglot-hook)
