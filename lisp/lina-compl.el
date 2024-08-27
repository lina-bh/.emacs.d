;; -*- lexical-binding: t; -*-
(use-package vertico
  :disabled t
  :ensure t
  :defines vertico-map
  :custom
  (vertico-mode t)
  (vertico-count-format nil)
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)))

(use-package helpful
  :disabled t
  :ensure t
  :custom (helpful-max-buffers nil)
  :hook (helpful-mode . show-paren-local-mode)
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-command] . helpful-command)
  (:map helpful-mode-map
        ("l" . quit-window)))

(use-package dumb-jump
  :disabled t
  :config
  (defun my-dumb-jump-c-mode-hook ()
    (add-hook 'xref-backend-functions #'dumb-jump-xref-activate 0 t))
  :config
  (dumb-jump-prefer-searcher 'rg)
  :hook (c-mode . my-dumb-jump-c-mode-hook))

(provide 'lina-compl)
