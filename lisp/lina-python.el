;; -*- lexical-binding: t; -*-
(eval-when-compile
  (require 'use-package)
  (require 'bind-key))

(defun restart-python ()
  (interactive)
  (let ((buffer (get-buffer "*Python*"))
	(kill-buffer-query-functions nil))
    (when buffer
      (kill-buffer buffer)))
  (call-interactively #'run-python))

(defun restart-python-send-current ()
  (interactive)
  (restart-python)
  (sleep-for 0 1)
  (save-some-buffers t
		     (lambda ()
		       (string= (file-name-extension buffer-file-name) "py")))
  (call-interactively 'python-shell-send-file))

(defun python-tab ()
  (interactive)
  (let ((initial-point (point)))
    (if (use-region-p)
	(save-excursion
	  (move-to-column 0)
	  (call-interactively 'python-indent-shift-right))
      (call-interactively 'python-indent-shift-right))
    (when (= (point) initial-point)
      (call-interactively 'indent-for-tab-command))
    (when (= (point) initial-point)
      (indent-line-to python-indent-offset))))

(use-package python
  :defer t
  :custom ((python-shell-interpreter-args "-i -q"))
  :bind ((:map python-mode-map
	       ("C-c C-l" . 'restart-python-send-current)
	       ;; ("<tab>" . 'python-tab)
	       ;; ("<backtab>" . 'python-indent-shift-left))
	       )))
