;; -*- lexical-binding: t; -*-
(defun indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max))))

(defun indent-last-sexp ()
  (interactive)
  (let ((saved-point (point)))
    (save-excursion
      (backward-sexp)
      (indent-region (point) saved-point))))

(defun split-and-follow-vertically ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))

(defun split-and-follow-horizontally ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))

(defun comment-line-in-place ()
  (interactive)
  (save-excursion (comment-line 1)))

(defun c-w-dwim (beg end)
  (interactive "r")
  (if (use-region-p)
      (kill-region beg end)
    (backward-kill-word 1)))

(defun just-save-some-buffers-thanks ()
  (interactive)
  (save-some-buffers t)
  (message "saved"))

(bind-key "C-x 2" #'split-and-follow-vertically)
(bind-key "C-x 3" #'split-and-follow-horizontally)

(bind-key "C-k" #'kill-whole-line)
(bind-key "C-w" #'c-w-dwim)

(bind-key "C-u" #'backward-kill-sentence minibuffer-local-map)

(bind-key "C-/" #'comment-line-in-place)

(bind-key "C-x C-g" #'keyboard-quit)
(bind-key "C-c C-g" #'keyboard-quit)

(bind-key "C-c k s" 'just-save-some-buffers-thanks)

(bind-key "M-i" 'completion-at-point)

(use-package elisp-mode
  :bind (:map emacs-lisp-mode-map
	      ("C-c b f" . indent-buffer)
	      ("C-c e f" . indent-last-sexp)
	      ("C-c C-e" . eval-last-sexp)))

(use-package ffap
  :init (ffap-bindings))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :custom (undo-tree-auto-save-history nil)
  :config (global-undo-tree-mode t))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config (which-key-mode t))

(provide 'lina-keys)
