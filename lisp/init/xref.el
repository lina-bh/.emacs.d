;; -*- lexical-binding: t; -*-
(use-package xref
  :ensure nil
  :custom
  (xref-prompt-for-identifier nil)
  :config
  (define-advice xref-matches-in-files
      (:around (xref-matches-in-files &rest args) ripgrep)
    (let ((xref-search-program (or (and (executable-find "rg" t)
                                        'ripgrep)
                                   'grep)))
      (apply xref-matches-in-files args)))
  :bind
  ("M-/" . xref-find-definitions))

(use-package dumb-jump
  :ensure t
  (xref-backend-functions (list
                           #'dumb-jump-xref-activate
                           #'etags--xref-backend)))
