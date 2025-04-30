(use-package man
  :ensure nil
  :config
  (define-advice Man-completion-table (:override (&rest _) empty)
    nil)
  (advice-add 'Man-notify-when-ready :override #'display-buffer))
