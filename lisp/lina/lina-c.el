;;; lina-c.el --- lina-c  -*- lexical-binding: t; -*-

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

(eval-when-compile
  (require 'cl-lib))

(defun lina-c-hook ()
  (setq-local whitespace-style '(face tabs tab-mark))
  (whitespace-mode t)
  (auto-insert))

(use-package c-ts-mode
  :ensure nil
  :custom (c-ts-indent-offset tab-width)
  :commands (c-ts-mode-set-global-style)
  :config
  (c-ts-mode-set-global-style 'linux)
  :hook (c-ts-mode-hook . lina-c-hook)
  :bind (:map c-ts-mode-map
              ("C-c C-c" . nil)))

(use-package cc-mode
  :ensure nil
  :custom ((c-default-style '((c-mode . "linux"))))
  :hook (c-mode-hook . lina-c-hook)
  :bind (:map c-mode-map
              ("C-c C-c" . nil)))

(use-package gud
  :ensure nil
  :functions gdb-display-buffer
  :custom ((gdb-display-io-buffer t)
           (gdb-debuginfod-enable-setting nil))
  :config
  (advice-add #'gdb-display-buffer :override #'display-buffer))

(provide 'lina-c)
;;; lina-c.el ends here
