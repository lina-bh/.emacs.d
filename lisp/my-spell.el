;; -*- lexical-binding: t; -*-
(autoload 'flyspell-correct-word "flyspell" nil t)

(with-eval-after-load 'flyspell
  (unbind-key [down-mouse-2] 'flyspell-mode-map)
  (unbind-key "C-M-i" 'flyspell-mode-map)
  (bind-keys :map 'flyspell-mode-map
             ([mouse-3] . flyspell-correct-word)))

(provide 'my-spell)
