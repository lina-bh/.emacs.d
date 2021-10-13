(use-package rjsx-mode
  :mode "\\.[m]?js\\'"
  :config (setq-default
	   js-chain-indent t
	   js-indent-level 2
	   js2-basic-offset 2)
  :bind (:map rjsx-mode-map
	      ("<" . nil)
	      ("C-d" . nil)
	      (">" . nil)))

(use-package add-node-modules-path
  :defer t
  :hook (((js2-mode rjsx-mode) . add-node-modules-path)))

(use-package prettier-js
  :defer t
  :hook (((js2-mode rjsx-mode) . prettier-js-mode)))

(use-package eglot
  :defer t
  :config (setq-default eglot-stay-out-of '(flymake)))
