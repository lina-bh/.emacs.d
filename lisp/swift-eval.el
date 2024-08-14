;; -*- lexical-binding: t; -*-
(defun swift-run-buffer ()
  (interactive)
  (if (eq major-mode 'swift-mode)
      (progn
        (save-buffer)
        (let (proc buf)
          (setq buf (get-buffer-create "*Swift Script*"))
          (setq proc (start-process "swift" buf "swift" buffer-file-name))
          (display-buffer buf))
    (user-error "only in swift-mode buffer")))
