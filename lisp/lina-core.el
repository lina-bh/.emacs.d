;; -*- lexical-binding: t; -*-


;; (if (executable-find "zsh" nil)
;;     (progn
;;       (defun my-shell-hook ()
;;         (setq-local comint-process-echoes t))
;;       (setopt explicit-shell-file-name (executable-find "zsh" nil)
;;               explicit-zsh-args '("--interactive")))
;;   (defun my-shell-hook () nil))



;; tls
;; gnutls-min-prime-bits 3072
;; network-security-level 'medium
(provide 'lina-core)
