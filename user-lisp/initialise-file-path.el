;;; initialise-file-path.el --- initialise-file-path  -*- lexical-binding: t; -*-

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

;;;###autoload
(defun initialise-file-path (name)
  (let ((remote (or (file-remote-p name)
                    "")))
    (setq name (abbreviate-file-name (file-local-name name)))
    (condition-case nil
        (let* ((cur (string-search "/" name))
               (els (list (substring name 0 cur))))
          (while (string-match (rx (group (? (not (any "/" alnum)))
                                          alnum)
                                   (0+ (not "/")) "/")
                               name cur)
            (push (substring name (match-beginning 1) (match-end 1)) els)
            (setq cur (match-end 0)))
          (concat remote
                  (string-join (nreverse (cons (substring name cur) els)) "/")))
      (t name))))

(ert-deftest initialise-file-path-home-config ()
  "Test abbreviation of home config path."
  (should (string= (initialise-file-path "~/.config/emacs/init.el")
                   "~/.c/e/init.el")))

(ert-deftest initialise-file-path-etc-config ()
  "Test abbreviation of /etc path."
  (should (string= (initialise-file-path "/etc/prog/conf.d/xyz.conf")
                   "/e/p/c/xyz.conf")))

(ert-deftest initialise-file-path-underscore ()
  "Test abbreviation of /etc path."
  (should (string= (initialise-file-path "/tmp/_underscore/xyz.conf")
                   "/t/_u/xyz.conf")))

(provide 'initialise-file-path)
;;; initialise-file-path.el ends here
