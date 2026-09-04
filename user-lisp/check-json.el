;;; check-json.el --- check-json  -*- lexical-binding: t; -*-

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

(require 'json)

;;;###autoload
(defun check-json ()
  "Check that the current buffer's contents is a valid JSON object."
  (interactive)
  (condition-case nil
      (save-excursion
        (goto-char (point-min))
        (json-read))
    (json-readtable-error (user-error "JSON read error")))
  nil)

;;;###autoload
(defun check-json-enable-in-buffer ()
  "Add `check-json' to `write-contents-functions' in current buffer.
If `json-read' signals an error when reading the buffer, it inhibits
saving the buffer to a file. This is intended to be run in a mode
hook. For example:

(add-hook \\='json-ts-mode-hook #\\='check-json-enable-in-buffer)

or:

(use-package json-ts-mode
  :hook (json-ts-mode . check-json-enable-in-buffer))"
  (add-hook 'write-contents-functions #'check-json nil t))

(defun check-json-benchmark-object-type ()
  (interactive)
  (message "%S"
           (let (results)
             (dolist (type '(alist plist hash-table) results)
               (push (cons type
                           (* 1000.0 (car
                                      (let ((json-object-type type))
                                        (benchmark-run 1000 (check-json))))))
                     results)))))

(provide 'check-json)
;;; check-json.el ends here
