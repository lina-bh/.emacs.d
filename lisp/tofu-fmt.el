;; -*- lexical-binding: t; -*-
(require 'reformatter)

(reformatter-define tofu-fmt
  :program "tofu"
  :args '("fmt" "-")
  :lighter " TofuFmt")

(provide 'tofu-fmt)
