(use-package ielm
  :ensure nil
  :commands ielm-interrupt
  :config
  (defun ielm-interrupt ()
    (interactive)
    (comint-skip-input)
    (ielm-return))
  :bind (:map ielm-map ("C-c" . ielm-interrupt)))
