;;; eval-expression-and-save.el --- eval-expression-and-save  -*- lexical-binding: t; -*-

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

(eval-when-compile
  (require 'subr-x)) ;; for `thread-first'

;;;###autoload
(defun eval-expression-and-save
    (exp &optional kill-value no-truncate char-print-limit)
  "Like `eval-expression', except save the value if prefixed."
  (interactive (advice-eval-interactive-spec
                (cadr (interactive-form #'eval-expression))))
  (let ((value (apply #'eval-expression
                      exp
                      nil
                      (and (>= emacs-major-version 26)
                           (list no-truncate char-print-limit)))))
    (when kill-value
      (kill-new (string-trim-right (pp-to-string value))))))

(provide 'eval-expression-and-save)
;;; eval-expression-and-save.el ends here
