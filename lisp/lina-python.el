;; -*- lexical-binding: t; -*-
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
  (if buffer-file-name
      (progn
	(save-some-buffers t
			   (lambda ()
			     (string= (file-name-extension buffer-file-name) "py")))
	(python-shell-send-file buffer-file-name))
    (python-shell-send-buffer)))

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
  :custom (python-shell-interpreter-args "-i -q")
  :bind (:map python-mode-map
	      ("C-c C-c" . 'restart-python-send-current)
	      ("<tab>" . 'python-tab)
	      ("<backtab>" . 'python-indent-shift-left))
  :bind (:map inferior-python-mode-map
	      ("<up>" . 'comint-previous-input)
	      ("<down>" . 'comint-next-input)))

(use-package python-black
  :ensure t
  :config
  (add-hook 'python-mode-hook
	    (defun lina/enable-black ()
	      (python-black-on-save-mode
	       (not (eq major-mode 'inferior-python-mode))))))

(provide 'lina-python)
