;;; kill-unfilled-region.el --- kill-unfilled-region  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Lina Bhaile <emacs-devel@linabee.uk>

;; Author: Lina Bhaile <emacs-devel@linabee.uk>
;; Package-Requires: ((emacs "31.0"))

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

;;;###autoload
(defun kill-unfilled-region (beg end)
  (interactive "r")
  (let ((buf (current-buffer)))
    (with-temp-buffer
      (insert-buffer-substring-no-properties buf beg end)
      (unfill-paragraph nil (point-min) (point-max))
      (kill-ring-save (point-min) (point-max))))
  (deactivate-mark)
  (message "Killed %S" (car kill-ring)))

(provide 'kill-unfilled-region)
;;; kill-unfilled-region.el ends here
