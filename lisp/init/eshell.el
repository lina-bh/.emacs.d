;; -*- lexical-binding: t; -*-
(use-package eshell
  :ensure nil
  :custom
  (eshell-confine-point-to-input t)
  (eshell-prefer-lisp-functions t)
  (eshell-ls-initial-args "-FHh")
  :bind
  ;; (:package esh-mode :map eshell-mode-map
  ;;           ("C-u" . eshell-kill-input))
  (:package esh-mode :map eshell-mode-map
            ("C-d" . eshell-send-eof-to-process)
            ("C-c" . eshell-interrupt-process))
  )
