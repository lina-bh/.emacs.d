;; -*- lexical-binding: t; -*-
(eval-when-compile
  (require 'use-package))
(require 'bind-key)

(use-package savehist
  :custom (savehist-mode t))
(use-package recentf
  :config (setq recentf-max-menu-items 160)
  :hook (find-file . recentf-mode)
  :bind ("C-x C-r" . #'recentf-open))
(use-package bookmark
  :custom
  (bookmark-save-flag 1)
  (bookmark-fringe-mark nil))
;; (use-package ffap
;;   :disabled t
;;   :bind (("C-x C-d" . #'dired-at-point)
;;          ("C-x C-f" . #'find-file-at-point)))

(use-package orderless
  :ensure
  :demand t
  :custom (orderless-component-separator " +\\|[-/]")
  :config (setq-default completion-styles '(orderless basic)))

(use-package icomplete
  :init
  (defun lina-icomplete-minibuf-hook ()
    (setq-local truncate-lines t
                completion-auto-help nil))
  :custom
  (icomplete-matches-format "")
  (icomplete-show-matches-on-no-input t)
  (icomplete-compute-delay 0.001)
  (fido-mode t)
  (fido-vertical-mode t)
  :hook (icomplete-minibuffer-setup . lina-icomplete-minibuf-hook))
;; (use-package vertico
;;   :disabled t
;;   :ensure
;;   :custom
;;   (vertico-count-format '("" . "%s/%s"))
;;   (vertico-mode t)
;;   :bind
;;   (:map vertico-map
;;         ("DEL" . #'vertico-directory-delete-char)))

(use-package marginalia
  :ensure
  :custom (marginalia-mode t))

(use-package consult
  :ensure
  :custom
  (consult-async-split-style nil)
  ;; (xref-show-xrefs-function #'consult-xref)
  ;; (xref-show-definitions-function #'consult-xref)
  :bind
  ;; ("C-s" . consult-line)
  ;; ("C-x p b" . consult-project-buffer)
  ;; ("C-x p f" . consult-find)
  ("C-x p g" . consult-ripgrep)
  ("C-h i" . consult-info)
  ("M-g g" . consult-goto-line)
  )
;; (use-package consult-org
;;   :disabled t
;;   :after org
;;   :demand t
;;   :bind (:map org-mode-map
;;               (("C-c C-j" . consult-org-heading))))
(use-package consult-imenu
  ;; :after imenu
  :bind
  ("M-g i" . consult-imenu)
  ("C-x p i" . consult-imenu-multi))

(use-package corfu
  :ensure
  :custom
  ;; (corfu-quit-no-match nil)
  (corfu-quit-at-boundary nil)
  (global-corfu-mode t))

(use-package embark-consult
  :ensure
  ;; :after consult
  ;; :demand t
  )
(use-package embark
  :ensure
  ;; :after embark-consult
  ;; :demand t
  :init
  (unbind-key "C-h" 'help-map)
  :custom
  (embark-indicators '(embark-minimal-indicator
                       embark-highlight-indicator
                       embark-isearch-highlight-indicator))
  (prefix-help-command #'embark-prefix-help-command)
  :bind
  ("C-." . embark-act)
  ("M-." . embark-dwim)
  (:map minibuffer-local-map
        ;; ("C-." . embark-act) TODO broken
        ("C-c" . embark-act)
        ;; ("C-e" . embark-export)))
        )
  )

;; (use-package which-key
;;   :disabled t
;;   :ensure
;;   :custom (which-key-mode t)
;;   :diminish which-key-mode)
