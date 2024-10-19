;; -*- lexical-binding: t; -*-
(autoload 'comint-previous-input "comint" nil t)
(autoload 'comint-next-input "comint" nil t)

(setopt comint-prompt-read-only t
        comint-scroll-to-bottom-on-input 'this)

(bind-keys :map 'comint-mode-map
           ("<up>" . comint-previous-input)
           ("<down>" . comint-next-input))

(add-hook 'shell-mode-hook
          (defun my-shell-mode-hook ()
            (setq-local comint-process-echoes t)))

(provide 'my-comint)