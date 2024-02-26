;; -*- lexical-binding: t; -*-
(eval-and-compile
  (require 'lina-package))

(use-package eglot
  :straight (:type built-in)
  :custom
  (eglot-ignored-server-capabilities '(:inlayHintProvider))
  (eglot-report-progress nil)
  :bind (:map eglot-mode-map
	      ("C-c 2" . #'eglot-rename)))

(use-package eglot-java
  :straight (:host github :repo "yveszoundi/eglot-java" :fork t)
  :defines eglot-java-mode-map
  :commands eglot-java-file-new eglot-java-run-main
  :custom (eglot-java-file-new-ask-type nil)
  :hook ((java-mode . eglot-java-mode)
	 (java-ts-mode . eglot-java-mode))
  :bind (:map eglot-java-mode-map
              ;; ("C-c 2" . #'eglot-java-rename)
	      ("C-c j n" . #'eglot-java-file-new)
	      ("C-c j r" . #'eglot-java-run-main)))

(use-package format-all
  :straight t
  :config
  (setopt format-all-formatters
          '(("Emacs Lisp" emacs-lisp)
	    ("Java" clang-format)
	    ("Python" ruff)
            ("Shell" (shfmt "-ci" "-i" "2"))))
  :bind (("C-c f" . #'format-all-region-or-buffer)))

(use-package ispell
  :defer t
  :custom (ispell-program-name "aspell")
	  (ispell-dictionary "british"))

(use-package flyspell
  :bind (:map flyspell-mode-map
	      ([mouse-3] . flyspell-correct-word))
  :config
  (unbind-key [down-mouse-2] 'flyspell-mode-map)
  (unbind-key "C-M-i" 'flyspell-mode-map))
