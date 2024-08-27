;; -*- lexical-binding: t; -*-
(autoload 'icomplete-fido-ret "icomplete" nil t)

(setopt fido-vertical-mode t
        icomplete-matches-format ""
        icomplete-show-matches-on-no-input t
        icomplete-compute-delay 0)

(add-hook 'icomplete-minibuffer-setup-hook
          (defun my-icomplete-hook ()
            (setq-local
             truncate-lines t
             completion-auto-help nil
             completion-styles (default-value 'completion-styles))))

(bind-keys :map 'icomplete-fido-mode-map
             ("TAB" . icomplete-fido-ret)
             ("<tab>" . icomplete-fido-ret))

(provide 'my-icomplete)
