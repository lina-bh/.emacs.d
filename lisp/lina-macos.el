;; -*- lexical-binding: t; -*-
(let ((monospace-spec '(:family "Monaco" :height 115)))
  (use-package emacs
      :if (eq system-type 'darwin)
      :custom
      (mouse-wheel-flip-direction t)
      :custom-face
      (default ((((type ns)) ,monospace-spec)))
      (fixed-pitch ((((type ns)) ,monospace-spec)))
      (variable-pitch ((((type ns)) (:family "Lucida Grande" :width regular :height 140))))
      (fill-column-indicator ((((type ns)) (:family "Menlo" :foreground "pink"
                                                    :background unspecified))))
      (tab-line ((((type ns)) (:family "Helvetica" :height 130))))))

(use-package dired
  :if (eq system-type 'darwin)
  :custom
  (dired-use-ls-dired nil)
  (dired-guess-shell-alist-user
   '(("\\.pdf\\'" "open -a Skim.app")
     ("" "open"))))

(use-package dash-at-point
  :if (eq system-type 'darwin)
  :bind ("C-h d" . dash-at-point))

(use-package org-agenda
  :custom
  (org-default-notes-file "~/Library/CloudStorage/Dropbox/org/inbox.org")
  (org-agenda-files
   `(,(expand-file-name "~/Library/CloudStorage/Dropbox/org"))))

;; https://www.emacswiki.org/emacs/MacOSXPlist
(use-package jka-compr
  :preface
  (defconst standard-jka-compr-compression-info-list
    (eval (car (get 'jka-compr-compression-info-list 'standard-value))))
  :custom
  (jka-compr-compression-info-list
   (cons ["\\.plist\\'"
          nil
          "plutil"
          ("-convert" "binary1" "-o" "-" "-")
          nil
          "plutil"
          ("-convert" "xml1" "-o" "-" "-")
          nil
          nil
          "bplist"]
         standard-jka-compr-compression-info-list))
  :config
  (jka-compr-update))
