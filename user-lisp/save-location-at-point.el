;;; save-location-at-point.el --- save-location-at-point  -*- lexical-binding: t; -*-

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

(autoload 'project-root "project")

;;;###autoload
(defun save-location-at-point (&optional column)
  (interactive "P")
  (let ((bfn (buffer-file-name)))
    (if (null bfn)
        (user-error "Buffer not associated with file")
      (let* ((root (and-let* ((project (project-current))
                              (root (project-root project)))
                     (expand-file-name root)))
             (relative (or (and root (file-relative-name bfn root))
                           (file-name-nondirectory bfn)))
             (loc (cond
                   ((use-region-p)
                    (let ((start
                           (save-excursion
                             (goto-char (region-beginning))
                             (line-number-at-pos)))
                          (end
                           (save-excursion
                             (goto-char (region-end))
                             (line-number-at-pos))))
                      (format "%s:%d-%d" relative start end)))
                   (column
                    (format "%s:%d:%d"
                            relative
                            (line-number-at-pos)
                            (current-column)))
                   (t
                    (format "%s:%d"
                            relative
                            (line-number-at-pos))))))
        (kill-new loc)
        (message "%s" loc)))))

(provide 'save-location-at-point)
;;; save-location-at-point.el ends here
