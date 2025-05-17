(use-package isearch
  :ensure nil
  :config
  (defun isearch-toggle-symbol-&-regexp- ()
    "Toggle between symbol and regexp searching."
    (interactive)
    (if (not isearch-mode)
        (message "can be called in isearch-mode")
      (if (not isearch-regexp)
          (isearch-toggle-regexp)
        (isearch-toggle-symbol))))
  :bind
  (("C-s" . isearch-forward-regexp)
   (:map isearch-mode-map
         ("TAB" . isearch-toggle-symbol-&-regexp-)
         ("<tab>" . isearch-toggle-symbol-&-regexp-)
         ("RET" . isearch-repeat-forward)
         ("<return>" . isearch-repeat-forward)
         ("ESC" . isearch-exit))))
