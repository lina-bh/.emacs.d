;;; lina-eshell.el --- lina-eshell  -*- lexical-binding: t; -*-

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

(defvar eshell-mode-map)
(declare-function eshell-kill-input "esh-mode")
(declare-function eshell-send-eof-to-process "esh-mode")
(declare-function eshell/rm@dont-delete-by-moving-to-trash nil)

(with-eval-after-load 'esh-mode
  (if (fboundp 'unix-word-rubout)
      (bind-key "C-w" #'unix-word-rubout eshell-mode-map)
    (bind-key "C-w" #'backward-kill-word eshell-mode-map))
  (bind-keys :package esh-mode :map eshell-mode-map
             ("C-u" . eshell-kill-input)
             ("C-d" . eshell-send-eof-to-process)))

(add-hook 'eshell-mode-hook (defun lina-eshell-hook ()
                              (electric-pair-local-mode -1)))

(with-eval-after-load 'eshell
  (setopt eshell-scroll-to-bottom-on-input t))

(with-eval-after-load 'em-unix
  (define-advice eshell/rm
      (:around (func &rest args) dont-delete-by-moving-to-trash)
    (let ((delete-by-moving-to-trash nil))
      (apply func args))))

(provide 'lina-eshell)
;;; lina-eshell.el ends here
