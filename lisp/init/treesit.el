(use-package treesit
  :if (treesit-available-p)
  :init
  (setq-default treesit-language-source-alist
                '((dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
                  (go "https://github.com/tree-sitter/tree-sitter-go" "master")
                  (rust "https://github.com/tree-sitter/tree-sitter-rust")
                  (json "https://github.com/tree-sitter/tree-sitter-json")
                  (bash "https://github.com/tree-sitter/tree-sitter-bash")))
  ;; (dolist (src treesit-language-source-alist)
  ;;   (setq lang (car src))
  ;;   (message "%S" lang)
  ;;   (unless (treesit-language-available-p lang)
  ;;     (treesit-install-language-grammar lang)))
  (require 'dockerfile-ts-mode)
  :custom
  ;; java typescript tsx lua
  (major-mode-remap-alist '((sh-mode . bash-ts-mode)
                            (js-json-mode . json-ts-mode)))
  :mode
  ("\\.go\\'" . go-ts-mode)
  :delight (dockerfile-ts-mode "Containerfile"))
