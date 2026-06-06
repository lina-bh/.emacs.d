;; -*- lexical-binding: t; -*-
(eval-when-compile
  (require 'cl-lib))

;;;###autoload
(defun push-point-to-register ()
  "Store point in an unused register from a-z."
  (interactive)
  (cl-labels ((f (c)
                (if (assoc c register-alist)
                    (if (>= c ?z)
                        (error "Ran out of keys")
                      (f (+ c 1)))
                  (point-to-register c)
                  c)))
    (message "%c" (f ?a))))
