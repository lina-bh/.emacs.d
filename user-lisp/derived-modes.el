;;; derived-modes.el --- derived-modes  -*- lexical-binding: t; -*-

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

;;;###autoload
(defun derived-modes? (mode &optional interactive)
  "What fucking modes does this major mode inherit?"
  (interactive
   (list major-mode t))
  (let (parents)
    (if (fboundp 'derived-mode-all-parents)
        (setq parents (reverse (derived-mode-all-parents mode)))
      (cl-labels ((f (modes)
                    (if-let* ((mode (car modes))
                              (parent (get mode 'derived-mode-parent)))
                        (f (cons parent modes))
                      modes)))
        (setq parents (f (list major-mode)))))
    (when interactive
      (message "%s" parents))
    parents))

(provide 'derived-modes)
;;; derived-modes.el ends here
