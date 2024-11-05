;; -*- lexical-binding: t; -*-
(require 'reformatter)

(reformatter-define nixfmt
  :program "nixfmt"
  :lighter " Fmt")

(provide 'nixfmt)
