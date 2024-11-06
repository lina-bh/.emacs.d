;; -*- lexical-binding: t; -*-
(require 'reformatter)

(reformatter-define yq-fmt
    :program "yamlfmt"
    :lighter " fmt"
    :args '("-in"))

(provide 'yq-fmt)
