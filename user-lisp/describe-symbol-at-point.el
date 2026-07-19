;;; describe-symbol-at-point.el --- describe-symbol-at-point  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Lina Bhaile <emacs-devel@linabee.uk>

;; Author: Lina Bhaile <emacs-devel@linabee.uk>
;; Package-Requires: ((emacs "28.1"))

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
(defun describe-symbol-at-point ()
  "Call `describe-symbol' on the symbol at point."
  (interactive)
  (let* ((name (or (thing-at-point 'symbol t)
                   (user-error "No symbol at point"))))
    (describe-symbol (unless (string= name "nil")
                       (or (intern-soft name)
                           (user-error "Unknown symbol: %s" name))))))

(defmacro ds@p--with-mocked-describe-symbol (&rest body)
  (declare (indent defun))
  `(let (args)
     (cl-letf (((symbol-function #'describe-symbol) (lambda (&rest args1)
                                                      (setq args args1))))
       ,@body)
     args))

(ert-deftest ds@p-function ()
  (with-temp-buffer
    (save-excursion
      (insert "car"))
    (ds@p--with-mocked-describe-symbol
      (describe-symbol-at-point)
      (should (eq (car args) 'car)))))

(ert-deftest ds@p-nil ()
  (with-temp-buffer
    (save-excursion
      (insert "nil"))
    (ds@p--with-mocked-describe-symbol
      (describe-symbol-at-point)
      (should (equal args '(nil))))))

(ert-deftest ds@p-unknown-symbol ()
  (with-temp-buffer
    (save-excursion
      (insert "surely-uninterned-symbol"))
    (ds@p--with-mocked-describe-symbol
      (should (equal (should-error (describe-symbol-at-point)
                                   :type 'user-error)
                     '(user-error "Unknown symbol: surely-uninterned-symbol")))
      (should-not args)
      (should-not (intern-soft "surely-uninterned-symbol")))))

(ert-deftest ds@p-no-symbol-at-point ()
  (with-temp-buffer
    (ds@p--with-mocked-describe-symbol
      (should (equal (should-error (describe-symbol-at-point)
                                   :type 'user-error)
                     '(user-error "No symbol at point")))
      (should-not args))))

(provide 'describe-symbol-at-point)
;;; describe-symbol-at-point.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("ds@p-" . "describe-symbol-at-point-"))
;; End:
