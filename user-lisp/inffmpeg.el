;;; inffmpeg.el --- Interface to ffmpeg.  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Lina Bhaile <emacs-devel@linabee.uk>

;; Author: Lina Bhaile <emacs-devel@linabee.uk>
;; Version: 2.0.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: multimedia, processes
;; URL: https://github.com/lina-bh/.emacs.d

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

;; Interactive user interface to ffmpeg, using widget.el and compile.el.

;; 1.0.1:
;; * License under DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE version 2.
;; 1.1.0:
;; * Fix `inffmpeg-default-directory' default value by adding a trailing slash.
;; * Set `default-directory' locally to `inffmpeg-default-directory'.
;; * Add buttons which call `read-file-name' and insert it into input/output
;;   fields.
;; 1.1.1:
;; * Fix an oversight where by the output file browse button changed the input
;;   file path.
;; * Expand file names when building the ffmpeg argument list, and when opening
;;   the file after completion.
;; 2.0.0:
;; * Relicense to GPL-3.0-or-later

;;; Code:

(require 'widget)

(eval-and-compile
  (require 'wid-edit))

(defvar inffmpeg-mode-map)

(defgroup inffmpeg nil "Interface to ffmpeg."
  :group 'external)

(defcustom inffmpeg-buffer-name "*inffmpeg*"
  "Name for inffmpeg's buffer."
  :type 'string
  :group 'inffmpeg)

(defcustom inffmpeg-default-input-arguments '(("-ss" "0"))
  "Default input arguments to ffmpeg."
  :type '(list string string)
  :group 'inffmpeg)

(defcustom inffmpeg-default-output-arguments '(("-map_metadata" "-1"))
  "Default output arguments to ffmpeg."
  :type '(list string string)
  :group 'inffmpeg)

(defcustom inffmpeg-default-directory "~/Videos/"
  "Default directory to work in.  NIL means `default-directory'."
  :type '(choice (const nil) directory))

(defvar-local inffmpeg--overwrite-checkbox)
(defvar-local inffmpeg--input-arguments-list)
(defvar-local inffmpeg--input-field)
(defvar-local inffmpeg--arguments-list)
(defvar-local inffmpeg--output-field)
(defvar-local inffmpeg--browse-file-checkbox)

(defvar-local inffmpeg--state)
(put 'inffmpeg--state 'permanent-local t)

(defun inffmpeg--update-state (&rest _ignore)
  "Update `inffmpeg--state' to the values of the buffer's widgets."
  (setq inffmpeg--state `((overwrite . ,(widget-value inffmpeg--overwrite-checkbox))
                          (input-arguments . ,(widget-value inffmpeg--input-arguments-list))
                          (input-file . ,(widget-value inffmpeg--input-field))
                          (output-arguments . ,(widget-value inffmpeg--arguments-list))
                          (output-file . ,(widget-value inffmpeg--output-field))
                          (browse-file . ,(widget-value inffmpeg--browse-file-checkbox)))))

(defun inffmpeg--command (&rest _ignore)
  "Build a list of arguments to ffmpeg from `inffmpeg--state'."
  (inffmpeg--update-state)
  (let (args)
    (push "ffmpeg" args)
    (let-alist inffmpeg--state
      (when .overwrite
        (push "-y" args))
      (dolist (pair .input-arguments)
        (setq args (cons (cadr pair) (cons (car pair) args))))
      (push "-i" args)
      (push (expand-file-name .input-file) args)
      (dolist (pair .output-arguments)
        (setq args (cons (cadr pair) (cons (car pair) args))))
      (push (expand-file-name .output-file) args))
    (nreverse args)))

(defun inffmpeg--run (&rest _ignore)
  "Run ffmpeg in a compilation buffer."
  (let* ((args (inffmpeg--command))
         (output-file (cdr (assq 'output-file inffmpeg--state)))
         (display-buffer-overriding-action '(display-buffer-below-selected))
         (compilation-process-setup-function
          (lambda ()
            (setq-local
             process-connection-type nil
             compilation-error-regexp-alist nil
             compilation-finish-functions
             (list (lambda (_buffer event)
                     (if (string= event "finished\n")
                         (browse-url (expand-file-name output-file))
                       (message "%S" event)))))))
         cmd)
    (compilation-start (string-join
                        (nreverse (dolist (arg args cmd)
                                    (push (concat "\"" arg "\"") cmd)))
                        " "))))

(defun inffmpeg--setup-buffer ()
  "Insert widgets into `inffmpeg-mode' buffer."
  (kill-all-local-variables)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  (setq-local header-line-format "ffmpeg")
  (let-alist inffmpeg--state
    (setq inffmpeg--overwrite-checkbox (widget-create 'checkbox .overwrite))
    (widget-insert " Overwrite\n")
    (widget-insert "Input arguments: \n")
    (setq inffmpeg--input-arguments-list
          (widget-create 'editable-list
                         :entry-format "%i%d %v"
                         :value .input-arguments
                         '(group
                           (editable-field :format "Arg: %v" :value "-")
                           (editable-field :format "Val: %v"))))
    (widget-insert "Input: ")
    (setq inffmpeg--input-field (widget-create 'file .input-file))
    (widget-create 'push-button
                   :notify (lambda (widget _event _unknown)
                             (let ((path (read-file-name "Input file: " nil nil t)))
                               (widget-value-set inffmpeg--input-field path)
                               (widget-apply inffmpeg--input-field :notify widget)))
                   "Select file...")
    (widget-insert "\nArguments: \n")
    (setq inffmpeg--arguments-list
          (widget-create 'editable-list
                         :entry-format "%i%d %v"
                         :value .output-arguments
                         '(group
                           (editable-field :format "Arg: %v" :value "-")
                           (editable-field :format "Val: %v"))))
    (widget-insert "Output: ")
    (setq inffmpeg--output-field (widget-create 'file .output-file))
    (widget-create 'push-button
                   :notify (lambda (widget _event _unknown)
                             (let ((path (read-file-name "Output file: ")))
                               (widget-value-set inffmpeg--output-field path)
                               (widget-apply inffmpeg--output-field :notify widget)))
                   "Select file...")
    (widget-insert "\n")
    (setq inffmpeg--browse-file-checkbox (widget-create 'checkbox .browse-file)))
  (widget-insert " Open file after completion\n")
  (widget-create 'push-button
                 :notify #'inffmpeg--run
                 "Run")
  (setq-local widget-global-map inffmpeg-mode-map
              completion-at-point-functions '(widget-completions-at-point)
              buffer-read-only nil
              quit-window-kill-buffer t)
  (use-local-map widget-keymap)
  (widget-setup))

(define-derived-mode inffmpeg-mode special-mode "Inffmpeg"
  :syntax-table nil
  :abbrev-table nil
  "Mode for inffmpeg's buffer."
  (inffmpeg--setup-buffer))

(keymap-set inffmpeg-mode-map "q" #'quit-window)

;;;###autoload
(defun inffmpeg ()
  "Interactive user interface for ffmpeg."
  (interactive)
  (with-current-buffer (get-buffer-create inffmpeg-buffer-name)
    (setq-local default-directory inffmpeg-default-directory
                inffmpeg--state
                `((overwrite . t)
                  (input-arguments . ,inffmpeg-default-input-arguments)
                  (input-file
                   .
                   ,(expand-file-name "input.mp4"))
                  (output-arguments . ,inffmpeg-default-output-arguments)
                  (output-file
                   .
                   ,(expand-file-name "output.mp4"))
                  (browse-file . t)))
    (inffmpeg-mode)
    (display-buffer (current-buffer))))

(provide 'inffmpeg)

;;; inffmpeg.el ends here
