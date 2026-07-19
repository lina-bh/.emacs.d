;;; autoload-insert.el --- autoload-insert  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Lina Bhaile <emacs-devel@linabee.uk>

;; Author: Lina Bhaile <emacs-devel@linabee.uk>
;; Package-Requires: ((emacs "24.4"))

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
(defun insert-autoload (func)
  "Insert a call to `autoload' for FUNC.
Errors if FUNC is itself an autoload.
When prefixed, insert the docstring, interactive and macro specs."
  (interactive "aInsert autoload for function: ")
  (when (autoloadp func)
    (error "%S is not loaded" func))
  (prin1 (append `(autoload ',func)
                 (list
                  (file-name-base (symbol-file func 'defun)))
                 (when current-prefix-arg
                   (list
                    (substring-no-properties (documentation func))
                    (commandp func)
                    (and (macrop func) ''macro))))
         (current-buffer)))

(provide 'autoload-insert)
;;; autoload-insert.el ends here
