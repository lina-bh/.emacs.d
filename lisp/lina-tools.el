;; -*- lexical-binding: t; -*-
(use-package magit-process
  :demand t
  :config (setopt magit-auto-revert-mode t))

(use-package magit
  :commands magit-dotfiles
  :custom
  (magit-display-buffer-function #'display-buffer)
  (magit-commit-show-diff nil)
  :config
  ;; https://github.com/magit/magit/issues/460#issuecomment-1435971974
  (defun magit-dotfiles ()
    "Magit on dotfiles repo for the duration of a recursive edit."
    (interactive)
    (let ((magit-git-global-arguments
           `(,(substitute-env-vars "--git-dir=$HOME/.dotfiles")
             ,(substitute-env-vars "--work-tree=$HOME")
             ,@magit-git-global-arguments)))
      (magit-status "~")
      (recursive-edit))))

(use-package dired
  :custom
  (delete-by-moving-to-trash t)
  (dired-recursive-deletes 'always)
  (dired-clean-confirm-killing-deleted-buffers nil)
  (dired-listing-switches "-aFlh")
  :hook (dired-mode . dired-hide-details-mode)
  :bind
  ("C-x d" . dired-jump)
  ("C-x C-d" . dired)
  (:map dired-mode-map
        ("<mouse-2>" . dired-mouse-find-file)))

(use-package helpful
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-command] . helpful-command)
  (:map embark-symbol-map
        ("h" . helpful-symbol)))

(use-package ange-ftp
  :custom
  (ange-ftp-default-user "anonymous")
  (ange-ftp-generate-anonymous-password "guest")
  (ange-ftp-try-passive-mode t))
