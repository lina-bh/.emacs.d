;; -*- lexical-binding: t; -*-
(use-package paren
  :init (show-paren-mode 0))
(use-package tab-line
  :custom (tab-line-new-button-show nil)
  :init
  (global-tab-line-mode t)
  (set-face-attribute 'tab-line nil))
(use-package desktop
  :init (desktop-save-mode t))
(use-package frame
  :init (blink-cursor-mode 0))
(use-package delsel
  :init (delete-selection-mode t))

(defun lina-prog-mode-hook ()
  ;; (electric-pair-local-mode t)
  (column-number-mode t)
  (display-line-numbers-mode t)
  (show-paren-local-mode t)
  (auto-save-mode 0)
  (setq-local indicate-empty-lines t
	      show-trailing-whitespace t
	      auto-save-default nil))
(use-package prog-mode
  ;; :after (simple display-line-numbers paren files)
  :hook (prog-mode . lina-prog-mode-hook))

(defun lina-text-mode-hook ()
  (visual-line-mode t)
  (variable-pitch-mode t))
(use-package text-mode
  ;; :after (simple face-remap)
  :hook (text-mode . lina-text-mode-hook))

(use-package eldoc
  :diminish eldoc-mode
  :custom (eldoc-echo-area-use-multiline-p nil))

(use-package markdown-mode
  :ensure t)

(use-package plantuml-mode
  :load-path "lisp/plantuml-mode"
  :custom
  (plantuml-default-exec-mode 'jar)
  :config
  (add-to-list 'plantuml-jar-args "-tpng"))

(use-package eldoc-overlay
  :ensure t
  :after quick-look
  :custom
  ((quick-peek-add-spacer nil)
   (quick-peek-position 'above)))

(use-package magit
  :ensure t
  :config (magit-auto-revert-mode t))

(use-package sql
  :init (setq-default sql-product 'sqlite))

(use-package polymode
  :config
  (eieio-oset-default pm-inner-chunkmode :adjust-face nil)
  (define-hostmode poly-python-hostmode
    :mode 'python-mode)
  (define-innermode poly-sql-innermode
    :mode 'sql-mode
    :head-matcher (rx
		   (= 3 (char "\"'"))
		   (zero-or-more (any "\t\n "))
		   (or "SELECT" "INSERT" "UPDATE" "DELETE" "CREATE"))
    :tail-matcher (rx (= 3 (char "\"'")))
    :head-mode 'host
    :tail-mode 'host)
  (define-polymode poly-python-sql-mode
    :hostmode 'poly-python-hostmode
    :innermodes '(poly-sql-innermode)))



(provide 'lina-modes)
