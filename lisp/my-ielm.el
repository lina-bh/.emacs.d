;; -*- lexical-binding: t; -*-
(autoload 'comint-skip-input "comint" nil t)
(autoload 'ielm-return "ielm" nil t)

(defun ielm-C-c ()
  (interactive)
  (comint-skip-input)
  (ielm-return))

(with-eval-after-load 'ielm
  (bind-keys :map 'ielm-map
             ("C-c C-c" . ielm-C-c)))

(provide 'my-ielm)
