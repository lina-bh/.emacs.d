;; -*- lexical-binding: t; -*-
(unless (memq 'Git vc-handled-backends)
  (push 'Git vc-handled-backends))

(use-package dart-mode)

(use-package dart-ts-mode
    :disabled t
    :vc (
         :url "https://github.com/50ways2sayhard/dart-ts-mode"
         :rev :newest))

(provide 'lina-dart)
