;; -*- lexical-binding: t; -*-
(defun split-and-follow-vertically ()
  (interactive)
  (let ((window (split-window-below)))
    (select-window window)))

(defun split-and-follow-horizontally ()
  (interactive)
  (let ((window (split-window-right)))
    (select-window window)))

(provide 'split-and-follow)
