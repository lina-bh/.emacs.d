;; -*- lexical-binding: t; -*-
(eval-and-compile
  (defvar straight-host-usernames '((github . "lina-bh")))
  (defvar straight-disable-native-compile t)
  (defvar bootstrap-version 7)
  (let ((bootstrap-file
         (expand-file-name
          "straight/repos/straight.el/bootstrap.el"
          (or (bound-and-true-p straight-base-dir)
              user-emacs-directory))))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage)))

(straight-use-package 'use-package)
(require 'bind-key)

(provide 'lina-package)
