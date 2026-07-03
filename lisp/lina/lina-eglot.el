;; -*- lexical-binding: t; -*-
(require 'eglot)
(setopt eglot-stay-out-of '(flymake)
        eglot-send-changes-idle-time 1
        eglot-server-programs
        '(((python-mode python-ts-mode) "ty" "server")
          (haskell-mode "haskell-language-server-wrapper" "--lsp")))
(defun lina/eglot-hook ()
  (eglot-inlay-hints-mode (if (derived-mode-p 'python-base-mode)
                              -1
                            t))
  (add-hook 'flymake-diagnostic-functions
            #'eglot-flymake-backend nil t)
  (flymake-mode t))
(add-hook 'eglot-managed-mode-hook #'lina/eglot-hook)
