;; -*- lexical-binding: t; -*-

(use-package ispell
  :config (setq ispell-program-name "aspell"
		ispell-dictionary "british"))

(use-package flyspell
  :hook (text-mode . flyspell-mode)
  :bind (:map flyspell-mode-map
	      ([mouse-3] . flyspell-correct-word))
  :config
  (unbind-key [down-mouse-2] 'flyspell-mode-map)
  (unbind-key "C-M-i" 'flyspell-mode-map))

(provide 'lina-spell)
