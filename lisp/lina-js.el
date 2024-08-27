;; -*- lexical-binding: t; -*-

;; https://github.com/doomemacs/doomemacs/blob/master/modules/lang/javascript/autoload.el#L35
(defun lina-add-node-modules-bin ()
  (interactive)
  (when-let ((buffer-file-name (buffer-file-name))
             (root (or (ignore-errors
                         (project-root (project-current)))
                       (locate-dominating-file buffer-file-name
                                               "node_modules/.bin")
                       (locate-dominating-file buffer-file-name
                                               "package.json"))))
    (make-local-variable 'exec-path)
    (push (expand-file-name "node_modules/.bin" root) exec-path)))

;; (use-package reformatter
;;   :commands prettier-buffer prettier-on-save-mode
;;   :config

;;   :hook (js-base-mode . prettier-on-save-mode))

(use-package js
  :defines js-mode-map
  :functions lina-js-mode-hook
  :custom (js-indent-level 2)
  :config
  (defun lina-js-mode-hook ()
    (make-local-variable 'electric-pair-text-pairs)
    (push '(?{ . ?}) electric-pair-text-pairs)
    (lina-add-node-modules-bin))
  :hook (js-base-mode . lina-js-mode-hook)
  :mode ((rx bos ".prettierrc" (opt ".json")) . js-json-mode)
  :bind (:map js-mode-map
              ("C-c f" . prettier-buffer)))

(use-package js-comint
  :commands js-comint-send-region js-comint-send-buffer
  :init
  (defun lina-js-comint-send ()
    "If region is active, send region, otherwise send buffer to inferior JS"
    (interactive)
    (call-interactively (if (use-region-p)
                            #'js-comint-send-region
                          #'js-comint-send-buffer)))
  :bind (:map js-base-mode-map
              ("C-x C-e" . js-comint-send-last-sexp)
              ("C-c C-e" . lina-js-comint-send)
              ("C-c C-r" . js-comint-reset-repl)))

(use-package web-mode
  :defines web-mode-map
  :init
  (defun lina-web-mode-hook ()
    (make-local-variable 'electric-pair-pairs)
    (push '(?< . ?>) electric-pair-pairs))
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-enable-auto-quoting nil)
  :hook (web-mode . lina-web-mode-hook)
  :mode
  ("\\.html\\'" . web-mode)
  :bind (:map web-mode-map
              ("C-c f" . prettier-buffer)))

(use-package css-mode
  :bind (:map css-mode-map
              ("C-c f" . prettier-buffer)))

