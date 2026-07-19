;;; lina-puni.el --- lina-puni  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Lina Bhaile <emacs-devel@linabee.uk>

;; Author: Lina Bhaile <emacs-devel@linabee.uk>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(when (featurep 'lina-smartparens)
  (error "load one or the other idiot"))

(use-package elec-pair
  :functions (electric-pair-inhibit-if-helps-balance)
  :custom ((electric-pair-mode t))
  :init
  (setopt electric-pair-inhibit-predicate
          (defun lina-electric-pair-inhibit (char)
            (or
             (electric-pair-inhibit-if-helps-balance char)
             (nth 3 (syntax-ppss))))))

(use-package puni
  :ensure t
  :defines (puni-mode-map)
  :functions (puni-kill-active-region
              puni-kill-line
              puni-backward-delete-char
              puni-delete-region)
  :config
  (defun lina/puni-c-w-dwim ()
    (interactive)
    (call-interactively (if (use-region-p)
                            #'puni-kill-active-region
                          #'backward-kill-sexp)))
  (defun lina/puni-kill-whole-line ()
    (interactive)
    (let ((kill-whole-line t))
      (move-beginning-of-line nil)
      (puni-kill-line)))
  (defun lina/puni-hungry-backward-delete-char ()
    "Hungrily delete backwards keeping expressions balanced.
If prefixed, or the region is active, or at the beginning of the line,
fall back to `puni-backward-delete-char' (which see)."
    (interactive)
    (if (or current-prefix-arg
            (use-region-p)
            (not (looking-back (rx line-start (+ blank))
                               (save-excursion
                                 (beginning-of-line)
                                 (point)))))
        (call-interactively #'puni-backward-delete-char)
      (puni-delete-region (1- (line-beginning-position)) (point))))
  :delight " Puni"
  :hook
  (lisp-data-mode-hook . puni-mode)
  (puni-mode-hook . electric-pair-local-mode)
  :bind
  (:map puni-mode-map
        ([remap puni-backward-delete-char]
         .
         lina/puni-hungry-backward-delete-char)
        ("C-k" . lina/puni-kill-whole-line)
        ("C-t" . puni-transpose)
        ("C-w" . lina/puni-c-w-dwim)
        ("C-c r" . puni-raise)
        ("C-c ." . puni-slurp-forward)
        ("C-c ," . puni-barf-forward)
        ("C-c s" . puni-splice)
        ("M-r" . puni-raise)
        ("M-<up>" . puni-backward-sexp-or-up-list)
        ("M-<left>" . puni-backward-sexp)
        ("M-<right>" . puni-forward-sexp)
        ("M-9" . puni-wrap-round))
  (:repeat-map lina-puni-barf-slurp-repeat-map
               ("." . puni-slurp-forward)
               ("," . puni-barf-forward)))

(provide 'lina-puni)
;;; lina-puni.el ends here
