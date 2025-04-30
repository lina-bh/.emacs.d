(use-package tab-line
  :ensure nil
  :custom
  (global-tab-line-mode t)
  (tab-line-new-button-show nil)
  (tab-line-close-button-show nil)
  (tab-line-switch-cycling nil)
  :custom-face
  (tab-line ((t (:height unspecified))))
  :config
  (define-advice tab-line-select-tab-buffer
      (:around (fun &rest args) dedicated)
    (let ((dedicated (window-dedicated-p)))
      (apply fun args)
      (set-window-dedicated-p (selected-window) dedicated))))
