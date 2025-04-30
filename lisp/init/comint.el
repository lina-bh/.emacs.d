(use-package comint
  :ensure nil
  :functions comint-skip-input
  :custom
  (comint-prompt-read-only t)
  (comint-scroll-to-bottom-on-input t)
  :bind (:map comint-mode-map
              ("C-u" . comint-kill-input)
              ("<up>" . comint-previous-input)
              ("<down>" . comint-next-input)))
