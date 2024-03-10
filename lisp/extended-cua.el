;; -*- lexical-binding: t; -*-
(require 'cua-base)

(define-key cua-global-keymap (kbd "C-z") #'undo)
(define-key cua-global-keymap (kbd "C-y") nil)
(define-key cua-global-keymap (kbd "C-s") #'save-buffer)
(define-key cua-global-keymap (kbd "C-f") #'isearch-forward)
(define-key cua-global-keymap (kbd "<C-return>") nil)
(define-key cua-global-keymap (kbd "C-a") #'mark-whole-buffer)
(when (require 'undo-tree nil t)
  (define-key cua-global-keymap (kbd "C-z") #'undo-tree-undo)
  (define-key cua-global-keymap (kbd "C-y") #'undo-tree-redo))

(provide 'extended-cua)
