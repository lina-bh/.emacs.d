;; -*- lexical-binding: t; -*-
(require 'package)

(setopt org-modules nil)

(let ((package-install-upgrade-built-in t)
      (org (package-get-descriptor 'org)))
  (unless (package-desc-dir org)
    (package-upgrade 'org)))
