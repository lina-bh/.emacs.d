;; -*- lexical-binding: t; -*-
(autoload 'dired-mouse-find-file "dired" nil t)
(autoload 'dired-hide-details-mode "dired" nil t)

(setopt dired-recursive-deletes 'always
        dired-clean-confirm-killing-deleted-buffers nil
        dired-listing-switches "-aFlh")

(add-hook 'dired-mode-hook #'dired-hide-details-mode)

(bind-keys :map 'dired-mode-map
           ("<mouse-2>" . dired-mouse-find-file))

(provide 'my-dired)
