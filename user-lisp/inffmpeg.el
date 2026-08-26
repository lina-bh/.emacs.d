;;; inffmpeg.el --- Interface to ffmpeg.  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Lina Bhaile <emacs-devel@linabee.uk>

;; Author: Lina Bhaile <emacs-devel@linabee.uk>
;; Version: 3.0.0
;; Package-Requires: ((emacs "29.1"))
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
;; 3.0.0:
;;   Major update.
;; * inffmpeg.el now requires Emacs 29 for `defvar-keymap'.
;; * Structured construction and value access of widgets. We now use an ordered
;;   alist of keys to constructor functions, call those to setup the buffer and
;;   store the widget objects in a new alist. We collect the state by looping
;;   over the widget alist and creating a new alist.
;; * `inffmpeg' now reads the paths of the input and output files when called.
;; * Preserve the unexpanded paths in the buffer and only expand them when we
;;   run ffmpeg.
;; * Fix a bug where the value of the browse-file checkbox was not respected.
;; * `inffmpeg-mode' no longer inherits from `special-mode'.
;; * Default input arguments now include "-vaapi_device" for hardware
;;   acceleration.
;; * The `default-directory' is now inherited from the input file.
;; * Instead of clobbering `compilation-process-setup-function' and
;;   `compilation-finish-functions', use `add-function' and `add-hook'.
;; * Stop echoing the process status message if ffmpeg didn't finish
;;   succesfully.
;; * Use `shell-quote-argument' for its purpose instead of cargo-culting shell
;;   escapes.
;; * Inherit from `widget-keymap' in `inffmpeg-mode-map' instead of overwriting
;;   the local map with `widget-keymap'.

;;; Code:

(require 'widget)

(eval-and-compile
  (require 'wid-edit))

(defgroup inffmpeg nil "Interface to ffmpeg."
  :group 'external)

(defcustom inffmpeg-buffer-name "*inffmpeg*"
  "Name for inffmpeg's buffer."
  :type 'string)

(defcustom inffmpeg-default-input-arguments
  '(("-vaapi_device" "/dev/dri/renderD128")
    ("-ss" "0"))
  "Default input arguments to ffmpeg."
  :type '(repeat (list string string)))

(defcustom inffmpeg-default-output-arguments
  '(("-map_metadata" "-1"))
  "Default output arguments to ffmpeg."
  :type '(repeat (list string string)))

(defun inffmpeg--notify-file-field (field mustmatch)
  "Create a lambda which reads a file name and sets the value of FIELD to it.
MUSTMATCH says whether the user needs to enter an already existing path (yes for
input, no for output)."
  (lambda (widget &rest _ignored)
    (let ((path (read-file-name "File: "
                                (file-name-directory (widget-value field))
                                nil
                                mustmatch)))
      (widget-value-set field path)
      (widget-apply field :notify widget))))

(defconst inffmpeg--template
  `((overwrite . ,(lambda (value)
                    (prog1
                        (widget-create 'checkbox value)
                      (widget-insert " Overwrite\n"))))
    (input-arguments
     .
     ,(lambda (value)
        (widget-insert "Input arguments: \n")
        (widget-create 'editable-list
                       :entry-format "%i%d %v"
                       :value value
                       '(group
                         (editable-field :format "Arg: %v" :value "-")
                         (editable-field :format "Val: %v")))))
    (input-file
     .
     ,(lambda (value)
        (widget-insert "Input: ")
        (let ((self (widget-create 'file value)))
          (widget-create 'push-button
                         :notify (inffmpeg--notify-file-field self t)
                         "Select file...")
          self)))
    (output-arguments
     .
     ,(lambda (value)
        (widget-insert "\nArguments: \n")
        (widget-create 'editable-list
                       :entry-format "%i%d %v"
                       :value value
                       '(group
                         (editable-field :format "Arg: %v" :value "-")
                         (editable-field :format "Val: %v")))))
    (output-file
     .
     ,(lambda (value)
        (widget-insert "Output: ")
        (let ((self (widget-create 'file value)))
          (widget-create 'push-button
                         :notify (inffmpeg--notify-file-field self nil)
                         "Select file...")
          self)))
    (browse-file . ,(lambda (value)
                      (widget-insert "\n")
                      (prog1
                          (widget-create 'checkbox value)
                        (widget-insert " Open file after completion\n")))))
  "Alist of keys to widget constructor functions, in order of appearance.")

(defvar-local inffmpeg--widgets nil
  "Alist of keys to widget objects.")
(defvar-local inffmpeg--state nil
  "Alist of keys to saved values.")
(put 'inffmpeg--state 'permanent-local t)

(defun inffmpeg--render ()
  "Set up inffmpeg's widgets.
For each constructor in `inffmpeg--template', call it and save its value
in `inffmpeg--widgets'."
  (setq-local inffmpeg--widgets nil)
  (dolist (pair inffmpeg--template)
    (let ((key (car pair))
          (constructor (cdr pair)))
      (push (cons key
                  (funcall constructor (cdr-safe (assq key inffmpeg--state))))
            inffmpeg--widgets))))

(defun inffmpeg--state ()
  "Create an alist mapping keys to current values in the buffer."
  (let (state)
    (dolist (pair inffmpeg--widgets)
      (push (cons (car pair)
                  (widget-value (cdr pair)))
            state))
    (setq inffmpeg--state state)))

(defun inffmpeg--command (state)
  "Create an ffmpeg argument list from STATE."
  (let (args)
    (push "ffmpeg" args)
    (let-alist state
      (when .overwrite
        (push "-y" args))
      (dolist (pair .input-arguments)
        ;; must go backwards
        (setq args (cons (cadr pair) (cons (car pair) args))))
      (push "-i" args)
      (push (expand-file-name .input-file) args)
      (dolist (pair .output-arguments)
        ;; id.
        (setq args (cons (cadr pair) (cons (car pair) args))))
      (push (expand-file-name .output-file) args))
    (nreverse args)))

(defun inffmpeg--compilation-setup-function (output-file)
  "Create a function which sets up the compilation buffer for ffmpeg to run in.
If OUTPUT-FILE is non-nil, add a hook to `compilation-finish-functions'
which calls `browse-url' on OUTPUT-FILE."
  (lambda ()
    (setq-local
     process-connection-type nil
     compilation-error-regexp-alist nil)
    (when output-file
      (add-hook 'compilation-finish-functions
                (lambda (_buffer event)
                  (when (string= event "finished\n")
                    (browse-url (expand-file-name output-file))))
                nil t))))

(defun inffmpeg-run ()
  "Run ffmpeg in a compilation buffer."
  (interactive)
  (let* ((state (inffmpeg--state))
         (args (inffmpeg--command state))
         (display-buffer-overriding-action '(display-buffer-below-selected))
         (compilation-process-setup-function compilation-process-setup-function))
    (add-function :after (var compilation-process-setup-function)
                  (inffmpeg--compilation-setup-function
                   (and (cdr (assq 'browse-file state))
                        (car (last args)))))
    (compilation-start (mapconcat #'shell-quote-argument args " "))))

(defvar-keymap inffmpeg-mode-map
  :parent widget-keymap
  "q" #'quit-window
  "C-c C-c" #'inffmpeg-run)

(define-derived-mode inffmpeg-mode nil "Inffmpeg"
  :syntax-table nil
  :abbrev-table nil
  "Mode for inffmpeg's buffer."
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  (inffmpeg--render)
  (widget-create 'push-button
                 :notify (lambda (&rest _ignored)
                           (inffmpeg-run))
                 "Run")
  (setq-local completion-at-point-functions '(widget-completions-at-point)
              quit-window-kill-buffer t)
  (widget-setup))

;;;###autoload
(defun inffmpeg (input-file output-file)
  "Interactive user interface for ffmpeg."
  (interactive
   (let* ((input-file (read-file-name "Input file: " nil nil t))
          (output-file (read-file-name "Output file: "
                                       (file-name-directory input-file)
                                       nil
                                       nil
                                       (concat (file-name-base input-file)
                                               "_2."
                                               (file-name-extension input-file)))))
     (list input-file output-file)))
  (with-current-buffer (get-buffer-create inffmpeg-buffer-name)
    (setq-local default-directory (file-name-directory input-file)
                inffmpeg--state
                `((overwrite . t)
                  (input-arguments . ,inffmpeg-default-input-arguments)
                  (input-file . ,input-file)
                  (output-arguments . ,inffmpeg-default-output-arguments)
                  (output-file . ,output-file)
                  (browse-file . t)))
    (inffmpeg-mode)
    (display-buffer (current-buffer))))

(provide 'inffmpeg)

;;; inffmpeg.el ends here
