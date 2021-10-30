;; -*- lexical-binding: t; -*-

(use-package ispell
  :config (setq ispell-program-name "aspell"
		ispell-dictionary "british"))

(use-package flyspell
  :hook (text-mode . flyspell-mode)
  :config
  (unbind-key "C-M-i" 'flyspell-mode-map))

(provide 'lina-spell)
