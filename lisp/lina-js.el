;; -*- lexical-binding: t; -*-
(use-package nodejs-repl)

(use-package js
  :ensure nil
  :custom
  (js-indent-level 2)
  (js-enabled-frameworks nil))

(provide 'lina-js)
