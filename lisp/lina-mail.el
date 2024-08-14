;; -*- lexical-binding: t; -*-
(setopt user-full-name "Lina Bhaile"
        user-mail-address "me@linabee.uk")

(use-package gnus
  :custom (gnus-select-method '(nnimap "imap.fastmail.com")))

(use-package elfeed)

(use-package elfeed-protocol
  :custom
  (elfeed-protocol-enabled-protocols '(newsblur))
  (elfeed-protocol-feeds '(("newsblur+https://linabee@newsblur.com"
                            :use-authinfo t)))
  :config
  (elfeed-protocol-enable))
