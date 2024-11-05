;; -*- lexical-binding: t; -*-
(require 'reformatter)

(defcustom biome-format-args
  '("--indent-style=space"
    "--indent-width=2"
    "--bracket-spacing=false"
    "--trailing-commas=es5"
    "--semicolons=as-needed"
    "--no-errors-on-unmatched"
    "--log-kind=compact")
  "")

(defmacro biome-format-define (lang)
  `(reformatter-define ,(intern (format "biome-format%s" lang))
     :program "biome"
     :args (append
            `("format"
              "--fix"
              ,(format "--stdin-file-path=%s.%s" input-file ,lang))
            biome-format-args)
     :lighter (format " Biome%s" (upcase ,lang))
     :input-file (if (file-remote-p buffer-file-name 'method)
                     (reformatter-temp-file ,lang)
                   (reformatter-temp-file-in-current-directory))))

(biome-format-define "js")
(biome-format-define "json")

(provide 'biome-format)
