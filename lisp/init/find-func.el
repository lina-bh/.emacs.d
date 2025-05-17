;; -*- lexical-binding: t; -*-
(use-package find-func
  :ensure nil
  :bind
  (:prefix "C-f" :prefix-map my-find-func-prefix-map
           ("f" . find-function)
           ("v" . find-variable)
           ("l" . find-library)
           ("k" . find-function-on-key)))
