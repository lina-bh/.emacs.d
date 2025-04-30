(use-package flyspell
  :ensure nil
  :bind (:map flyspell-mode-map
              ("C-M-i" . nil)
              ([down-mouse-2] . nil)
              ([mouse-3] . flyspell-correct-word)))
