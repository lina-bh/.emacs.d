;;; interprogram-copy-from-kill-ring.el --- interprogram-copy-from-kill-ring  -*- lexical-binding: t; -*-

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
(defun interprogram-copy-from-kill-ring ()
  "Select a stretch of previously killed text and load it into the clipboard."
  (interactive)
  (unless interprogram-cut-function
    (user-error "%S is nil" 'interprogram-cut-function))
  (funcall interprogram-cut-function
           (read-from-kill-ring "Copy from kill-ring: ")))

(provide 'interprogram-copy-from-kill-ring)
;;; interprogram-copy-from-kill-ring.el ends here
