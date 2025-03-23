;; -*- lexical-binding: t; -*-
(defun delete-visited-file ()
  "Delete the file in the current buffer."
  (interactive)
  (let* ((buffer (current-buffer))
         (file-name (buffer-file-name buffer)))
    (if file-name
        (when (y-or-n-p (format "Delete %s?" file-name))
          (funcall-interactively #'delete-file file-name)
          (kill-buffer buffer))
      (message "Buffer not visiting any file"))))

(defun which ()
  "Where the fuck is this command on the PATH?"
  (interactive)
  (if-let* ((command (read-shell-command "Which command: "))
            (path (executable-find command)))
      (message "%s" path)
    (message "%s is not recognized as an internal or external command, operable\
 program or batch file." command)))

(defun major-mode? ()
  "What fucking mode is this?"
  (interactive)
  (message "%s" major-mode))

(defun derived-modes? (mode &optional interactive)
  "What fucking modes does this major mode inherit?"
  (interactive
   (list major-mode t))
  (cl-labels ((f (modes)
                (if-let* ((mode (car modes))
                          (parent (get mode 'derived-mode-parent)))
                    (f (cons parent modes))
                  modes)))
    (let ((parents (f (list mode))))
      (when interactive
        (message "%s" parents))
      parents)))

(defun split-and-follow-vertically ()
  (interactive)
  (let ((window (split-window-below)))
    (select-window window)))

(defun split-and-follow-horizontally ()
  (interactive)
  (let ((window (split-window-right)))
    (select-window window)))

(defun c-w-dwim (&optional prefix)
  (interactive "p")
  (if (use-region-p)
      (kill-region (region-beginning)
                   (region-end))
    (backward-kill-word (or prefix 1))))

(defun local-eval-defun ()
  (interactive)
  (call-interactively (keymap-lookup (current-local-map) "C-M-x")))

(defun colorise ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

(defun ansible-doc (module)
  (interactive
   (list
    (substring (thing-at-point 'filename t) 0 -1)))
  (let ((buffer-name (format "*%s*" module)))
    (or (and (get-buffer buffer-name)
             (display-buffer buffer-name))
        (progn
          (async-shell-command (format "ansible-doc '%s'" module) buffer-name)
          (colorise)))))

(bind-keys ("C-w" . c-w-dwim)
           ("C-x 2" . split-and-follow-vertically)
           ("C-x 3" . split-and-follow-horizontally))

(provide 'my-funs)
