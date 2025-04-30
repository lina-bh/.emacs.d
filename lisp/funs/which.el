;; -*- lexical-binding: t; -*-
(defun which ()
  "Where the fuck is this command on the PATH?"
  (interactive)
  (if-let* ((command (read-shell-command "Which command: "))
            (path (executable-find command)))
      (message "%s" path)
    (message "%s is not recognized as an internal or external command, operable\
 program or batch file." command)))
