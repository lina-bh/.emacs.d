(use-package dired
  :ensure nil
  :custom
  (delete-by-moving-to-trash t)
  (dired-recursive-deletes 'always)
  (dired-clean-confirm-killing-deleted-buffers nil)
  (dired-listing-switches "-aFlh")
  :config
  (define-advice dired-post-do-command (:after (&rest _) unmark)
    (dired-unmark-all-marks))
  :hook (dired-mode . dired-hide-details-mode)
  :bind
  ("C-x d" . dired-jump)
  (:map dired-mode-map
        ("<mouse-2>" . dired-mouse-find-file)))
