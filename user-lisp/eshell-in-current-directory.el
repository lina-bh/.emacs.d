;;; eshell-in-current-directory.el --- eshell-in-current-directory  -*- lexical-binding: t; -*-

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

(autoload 'eshell/cd "em-dirs")
(autoload 'eshell-reset "esh-mode")

;;;###autoload
(defun eshell-in-buffer-directory ()
  "Create or switch to `eshell' in the `default-directory' of the selected buffer."
  (interactive)
  (let ((bufdir default-directory))
    (with-current-buffer (eshell)
      (unless (string= bufdir default-directory)
        (eshell/cd `(,bufdir))
        (eshell-reset)))))

(provide 'eshell-in-current-directory)
;;; eshell-in-current-directory.el ends here
