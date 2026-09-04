;;; podman-images.el --- podman-images  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Lina Bhaile <emacs-devel@linabee.uk>

;; Author: Lina Bhaile <emacs-devel@linabee.uk>
;; Version: 1.0.0
;; Package-Requires: ((emacs 31.0))

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

(require 'tramp-container) ;; for `tramp-podman-program'

(defconst podman-images-buffer-name "*podman-images*"
  "Name of buffer for `podman-images-ls'.")

(defconst podman-images-delete-tag (propertize "D "
                                               'fontified t
                                               'face 'dired-mark))

(defsubst podman-images-parse-reference (reference)
  "Parse a container image REFERENCE into a list of name, tag and digest."
  (if (null (string-match (rx (group (+ (not (or ":" "@"))))
                              (opt ":" (group (+ (not "@"))))
                              (opt "@" (group (+ nonl))))
                          reference))
      (error "%s" reference)
    (list (match-string-no-properties 1 reference)
          (match-string-no-properties 2 reference)
          (match-string-no-properties 3 reference))))

(defun podman-images-mark ()
  "Mark image at point for removal."
  (interactive nil podman-images-mode)
  (tabulated-list-put-tag (propertize "D"
                                      'fontified t
                                      'face 'dired-mark)
                          t))

(defun podman-images-get-tag ()
  "Get the tag in the padding area of the current line.
`tabulated-list-mode' lets you put, but not get the tag. Make it make sense!"
  (save-excursion
    (beginning-of-line)
    (buffer-substring (point) (+ (point) tabulated-list-padding))))

(defun podman-images-remove-marked ()
  "Delete images marked for removal."
  (interactive nil podman-images-mode)
  (let (remove)
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (point-max))
        (when (string= (podman-images-get-tag) podman-images-delete-tag)
          (push (tabulated-list-get-id) remove))
        (forward-line)))
    (if (null remove)
        (user-error "No marks")
      (when (yes-or-no-p (concat (string-join remove "\n")
                                 "\nRemove these images?"))
        (make-process :name "podman-image-rm"
                      :buffer nil
                      :command (append
                                (cons tramp-podman-program
                                      '("image"
                                        "rm"))
                                remove)
                      :connection-type 'pipe
                      :filter
                      (lambda (_proc string)
                        (if (string-prefix-p "Error: " string)
                            (user-error "%s" string)
                          (message "%s" string))))))))

(defun podman-images-unmark-all ()
  "Remove all marks."
  (interactive nil podman-images-mode)
  (tabulated-list-clear-all-tags))

(defvar-keymap podman-images-mode-map
  :doc "Keymap for `podman-images-mode'."
  :parent tabulated-list-mode-map
  "d" #'podman-images-mark
  "x" #'podman-images-remove-marked
  "U" #'podman-images-unmark-all)

(defmacro podman-images-make-sorter (idx)
  "Create a sort function for two rows' column IDX by text property :sort-by."
  `(lambda (x y)
     (< (get-text-property 0 :sort-by (aref (cadr x) ,idx))
        (get-text-property 0 :sort-by (aref (cadr y) ,idx)))))

(define-derived-mode podman-images-mode tabulated-list-mode "Images" nil
  :syntax-table nil
  :abbrev-table nil
  (make-local-variable 'revert-buffer-function)
  (setq revert-buffer-function (lambda (&rest _ignored)
                                 (podman-images-refresh))
        tabulated-list-padding 2
        tabulated-list-format `[("REPOSITORY" 32 t)
                                ("TAG" 13 nil)
                                ("IMAGE ID" 13 nil)
                                ("CREATED" 13
                                 ,(podman-images-make-sorter 3))
                                ("SIZE" 7
                                 ,(podman-images-make-sorter 4)
                                 :right-align t)
                                ("DIGEST" 0 nil)])
  (tabulated-list-init-header))

(defun podman-images-refresh ()
  "Refresh the list of images."
  (interactive nil podman-images-mode)
  (let ((buffer (get-buffer-create " *podman-image-ls*")))
    (with-current-buffer buffer
      (erase-buffer)
      (make-process
       :name "podman-image-ls"
       :buffer (current-buffer)
       :command (list tramp-podman-program
                      "image"
                      "ls"
                      "--format"
                      "json")
       :noquery t
       :connection-type 'pipe
       :sentinel
       (lambda (process event)
         (if (not (string= event "finished\n"))
             (error "%s %s"
                    (string-join (process-command process))
                    (string-trim-right event)))
         (let* ((images (with-current-buffer (process-buffer process)
                          (goto-char (point-min))
                          (json-parse-buffer
                           :object-type 'alist
                           :array-type 'list))))
           (with-current-buffer (get-buffer podman-images-buffer-name)
             (setq tabulated-list-entries nil)
             (dolist (image images)
               (let-alist image
                 (let ((parsed (and-let* ((ref (car (last .Names))))
                                 (podman-images-parse-reference ref))))
                   (push (list .Id
                               (vector (or (car parsed) "<none>")
                                       (or (cadr parsed) "<none>")
                                       .Id
                                       (propertize (concat (seconds-to-string
                                                            (- (float-time)
                                                               .Created)
                                                            t)
                                                           " ago")
                                                   :sort-by .Created)
                                       (propertize (file-size-human-readable
                                                    .Size
                                                    nil
                                                    " "
                                                    "B")
                                                   :sort-by .Size)
                                       .Digest))
                         tabulated-list-entries)))))
           (tabulated-list-print t)))))))

(defun podman-images ()
  "List Podman images.
Calls `tramp-podman-program'."
  (interactive)
  (let ((buffer (get-buffer-create podman-images-buffer-name)))
    (with-current-buffer buffer
      (podman-images-mode))
    (podman-images-refresh)
    (display-buffer buffer)))

(provide 'podman-images)
;;; podman-images.el ends here
