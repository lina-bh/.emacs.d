;; -*- lexical-binding: t; -*-
(eval-when-compile
  (require 'use-package))
(require 'bind-key)

(use-package eglot
  :custom
  (eglot-ignored-server-capabilities '(:inlayHintProvider))
  (eglot-report-progress nil)
  :bind (:map eglot-mode-map
	      ("C-c 2" . eglot-rename)))

(use-package eglot-java
  :disabled t
  :custom (eglot-java-file-new-ask-type nil)
  :hook (java-ts-mode . eglot-java-mode)
  :bind (:map eglot-java-mode-map
	      ("C-c j n" . #'eglot-java-file-new)
	      ("C-c j r" . #'eglot-java-run-main)))

(use-package format-all
  :ensure
  :config
  (setopt format-all-formatters
          '(("Emacs Lisp" emacs-lisp)
	    ("Java" clang-format)
	    ("Python" ruff)
            ("Shell" (shfmt "-ci" "-i" "2"))))
  :bind ("C-c f" . format-all-region-or-buffer))

(use-package ispell
  :custom
  (ispell-program-name "aspell")
  (ispell-dictionary "british"))

(use-package flyspell
  :config
  (unbind-key [down-mouse-2] 'flyspell-mode-map)
  (unbind-key "C-M-i" 'flyspell-mode-map)
  :bind (:map flyspell-mode-map
	      ([mouse-3] . flyspell-correct-word)))
