;; -*- lexical-binding: t; -*-
(eval-when-compile
  (require 'cl-lib))

(use-package mu4e
  :custom
  (mail-user-agent 'mu4e-user-agent)
  (message-dont-reply-to-names #'mu4e-personal-or-alternative-address-p)
  (mm-discouraged-alternatives '("text/html"))
  (mu4e-sent-folder "/Sent")
  (mu4e-trash-folder "/Trash")
  (mu4e-refile-folder "/Archive")
  (mu4e-drafts-folder "/Drafts")
  (mu4e-get-mail-command (format "mbsync --verbose %s" user-mail-address))
  (mu4e-completing-read-function #'completing-read)
  (mu4e-read-option-use-builtin nil)
  (mu4e-change-filenames-when-moving t)
  (mu4e-maildir-shortcuts `((:maildir "/INBOX" :key ?i)
                            (:maildir ,mu4e-sent-folder :key ?s)
                            (:maildir ,mu4e-trash-folder :key ?t)
                            (:maildir ,mu4e-drafts-folder :key ?d)
                            (:maildir ,mu4e-refile-folder :key ?a)))
  (mu4e-bookmarks `(( :name "INBOX"
                      :key ?i
                      :query ,(format "maildir:/INBOX AND NOT (flag:trashed OR maildir:%s)"
                                      mu4e-trash-folder)
                      :favorite t)))
  (mu4e-split-view 'single-window)
  (mu4e-modeline-mode nil)
  (mu4e-confirm-quit nil)
  :config
  (defun lina/mu4e-compose-mbsync-push-sent ()
    (let (mu4e-get-mail-command)
      (catch t
        (setq mu4e-get-mail-command (format "mbsync --push push:%s"
                                            (substring (cl-case mu4e-message-post-action
                                                         (postpone mu4e-drafts-folder)
                                                         (send mu4e-sent-folder)
                                                         (t (throw nil nil)))
                                                       1)))
        (mu4e-update-mail-and-index t))))
  ;; :hook
  ;; (mu4e-compose-post-hook . lina/mu4e-compose-mbsync-push-sent)
  :bind
  (("C-x m" . mu4e-jump-to-favorite)
   (:map mu4e-headers-mode-map
         ("q" . mu4e-quit))
   (:map mu4e-compose-minor-mode-map
         ("R" . mu4e-compose-wide-reply))))

(use-package message
  :ensure nil
  :custom
  (message-kill-buffer-on-exit t)
  (message-hidden-headers '("^References:")))

(use-package smtpmail
  :ensure nil
  :custom
  (message-send-mail-function 'smtpmail-send-it)
  (smtpmail-default-smtp-server "smtp.fastmail.com")
  (smtpmail-servers-requiring-authorization ".*")
  (smtpmail-smtp-service 465)
  (smtpmail-stream-type 'tls))

(use-package gnus
  :ensure nil
  :custom
  (gnus-select-method '(nnmaildir "" (directory "~/Maildir/")))
  (gnus-secondary-select-methods '((nntp "news.gmane.io")))
  (gnus-use-full-window nil)
  (gnus-permanently-visible-groups (rx (or "INBOX"
                                           "Archive"
                                           "Sent")))
  (gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date))
  (gnus-newsgroup-maximum-articles gnus-large-newsgroup)
  :hook (gnus-article-mode-hook . scroll-lock-mode)
  :bind
  (:map gnus-article-mode-map
        ("n" . gnus-summary-next-article)
        ("p" . gnus-summary-prev-article)
        ("q" . gnus-article-show-summary))
  (:map gnus-summary-mode-map
        ("n" . gnus-summary-next-article)
        ("p" . gnus-summary-prev-article)
        ("b" . gnus-article-prev-page)))

(provide 'lina-mail)
