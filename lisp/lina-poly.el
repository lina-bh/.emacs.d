;; -*- lexical-binding: t; -*-
(use-package polymode
  :config
  (setq polymode-move-these-minor-modes-from-old-buffer '(visual-line-mode
                                                          tab-line-mode)))

(defun lina-turn-off-display-fill-indicator-mode (type)
  (display-fill-column-indicator-mode -1))

(use-package poly-noweb
  :after polymode
  :init
  (defun lina-poly-noweb-electric-<< (arg)
    "Auto insert noweb chunk if at bol followed by white space.
If given an numerical argument, it simply insert `<'. Otherwise,
if at the beginning of a line in a host chunk insert \"<<>>=\", a
closing \"@\" and a newline if necessary."
    (interactive "P")
    (if (or arg (car (pm-innermost-span)))
        (self-insert-command (if (numberp arg) arg 1))
      (if (not (looking-back "^[ \t]*<"))
          (self-insert-command 1)
        (insert "<")
        (save-excursion
          (insert ">>=\n@ ")
          (unless (looking-at "\\s *$")
            (newline))))))
  :config
  (when (package-installed-p 'auctex)
    (oset poly-latex-hostmode mode 'LaTeX-mode))
  (oset poly-noweb-innermode adjust-face '(:inherit fixed-pitch))
  (object-add-to-list poly-noweb-innermode 'init-functions
                      #'lina-turn-off-display-fill-indicator-mode t)
  (advice-add #'poly-noweb-electric-< :override #'self-insert-command))

;; (use-package polymode
;;   :config
;;   (eieio-oset-default pm-inner-chunkmode :adjust-face nil)
;;   (define-hostmode poly-python-hostmode
;;     :mode 'python-mode)
;;   (define-innermode poly-sql-innermode
;;     :mode 'sql-mode
;;     :head-matcher (rx
;; 		   (= 3 (char "\"'"))
;; 		   (zero-or-more (any "\t\n "))
;; 		   (or "SELECT" "INSERT" "UPDATE" "DELETE" "CREATE"))
;;     :tail-matcher (rx (= 3 (char "\"'")))
;;     :head-mode 'host
;;     :tail-mode 'host)
;;   (define-polymode poly-python-sql-mode
;;     :hostmode 'poly-python-hostmode
;;     :innermodes '(poly-sql-innermode)))
