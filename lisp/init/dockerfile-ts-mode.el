;; -*- lexical-binding: t; -*-
(require 'mode-local)

(use-package dockerfile-ts-mode
  :config
  (setq-mode-local dockerfile-ts-mode
                   indent-line-function
                   (defun indent-dockerfile ()
                     (let ((char (save-excursion
                                   (forward-line -1)
                                   (end-of-line)
                                   (preceding-char))))
                       (if (= char ?\\)
                           (indent-to-column 4)
                         'no-indent))))
  :delight (dockerfile-ts-mode "Containerfile")
  :mode ("\\(?:Containerfile\\(?:\\..*\\)?\\|\\.[Cc]ontainerrfile\\)\\'"))
