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
