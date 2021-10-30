;; -*- lexical-binding: t; -*-
(use-package org
  :defer t
  :init (setq org-export-backends '(html latex)
	      org-modules nil)
  :config (setq-default org-adapt-indentation nil
			org-descriptive-links nil))

(use-package org-wc
  :load-path "site-lisp/org-wc"
  :after org
  :bind (:map org-mode-map
	      ("C-c w" . org-wc-display)))

(provide 'lina-org)
