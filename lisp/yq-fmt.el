;; -*- lexical-binding: t; -*-
(require 'reformatter)

(reformatter-define yq-fmt
  :program "yq"
  :lighter " yq"
  :args '("--yml-output"
          "--yml-out-ver=1.2"
          "--explicit-start"
          "--indentless"
          "."))

(provide 'yq-fmt)
