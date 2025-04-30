(use-package autoinsert
  :ensure nil
  :custom
  (auto-insert-directory (locate-user-emacs-file "auto-insert/"))
  (auto-insert-alist `((,(rx "." (or "tex" "ltx") string-end) . "latex")
                       ("\\.el\\'" . ,(lambda ()
                                        (elisp-enable-lexical-binding))))))
