;; -*- lexical-binding: t; -*-
(with-eval-after-load 'tab-line
  (define-advice tab-line-select-tab-buffer
      (:around (fun &rest args) dedicated)
    (let ((dedicated (window-dedicated-p)))
      (apply fun args)
      (set-window-dedicated-p (selected-window) dedicated))))

(setopt global-tab-line-mode t
        tab-line-new-button-show nil
        tab-line-close-button-show nil
        tab-line-switch-cycling nil)

(provide 'my-tab-line)
