;;; which.el --- which  -*- lexical-binding: t; -*-

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

;;;###autoload
(defun which (programname &optional prefix)
  "Echo the absolute path to PROGRAMNAME, if it's in `exec-path'.
Prefix arguments work as follows:

 `prefix' & 1    if current buffer is remote, don't search the remote system.
 `prefix' & 2    push to `kill-ring'.

Wraps `executable-find' (which see)."
  (interactive "MProgram: \np")
  (let (file)
    (if (not (setq file (executable-find programname (xor
                                                      (= (logand prefix 1) 1)
                                                      (and (buffer-file-name)
                                                           (file-remote-p
                                                            (buffer-file-name)
                                                            nil t))))))
        (message "%s not found" programname)
      (when (= (logand prefix 2) 2)
        (kill-new file))
      (message "%s" file))))

(provide 'which)
;;; which.el ends here
