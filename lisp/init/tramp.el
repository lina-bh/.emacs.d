;; -*- lexical-binding: t; -*-
(use-package ange-ftp
  :ensure nil
  :custom
  (ange-ftp-default-user "anonymous")
  (ange-ftp-generate-anonymous-password "guest")
  (ange-ftp-try-passive-mode t))

(use-package tramp
  :ensure nil
  :custom
  (tramp-verbose 2)
  :init
  (tramp-enable-method 'distrobox))
