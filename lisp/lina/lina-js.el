;;; lina-js.el --- lina-js  -*- lexical-binding: t; -*-

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

(use-package js
  :ensure nil
  :custom (js-indent-level 2)
  :config
  (defun lina-js-hook ()
    (setq-local eldoc-echo-area-use-multiline-p t))
  :hook ((js-base-mode-hook typescript-ts-base-mode-hook) . lina-js-hook)
  :mode ((rx ".conflist" eos) . js-json-mode))

(provide 'lina-js)
;;; lina-js.el ends here
