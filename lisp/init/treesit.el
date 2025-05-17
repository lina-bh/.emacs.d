;; -*- lexical-binding: t; -*-
(use-package treesit
  :if (treesit-available-p)
  :commands treesit-install-all-grammars-
  :init
  (setq-default treesit-language-source-alist
                '((dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
                  (go "https://github.com/tree-sitter/tree-sitter-go" "master")
                  (rust "https://github.com/tree-sitter/tree-sitter-rust")
                  (json "https://github.com/tree-sitter/tree-sitter-json")
                  (bash "https://github.com/tree-sitter/tree-sitter-bash")
                  (html "https://github.com/tree-sitter/tree-sitter-html")
                  (astro "https://github.com/virchau13/tree-sitter-astro")))
  :config
  (defun treesit-install-all-grammars- ()
    (interactive)
    (dolist (src treesit-language-source-alist)
      (let ((lang (car src)))
        (unless (treesit-language-available-p lang)
          (message "Installing %s..." lang)
          (treesit-install-language-grammar lang)))))
  :custom
  ;; java typescript tsx lua
  (major-mode-remap-alist '((sh-mode . bash-ts-mode)
                            (js-json-mode . json-ts-mode)))
  :mode
  ("\\.go\\'" . go-ts-mode))
