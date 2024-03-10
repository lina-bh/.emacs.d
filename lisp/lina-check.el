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

(defvar sdkman-dir (expand-file-name "~/.sdkman")
  "Path to SDKMAN!, a version manager for Java virtual machines and tools.")

(defun sdkman-home ()
  "Call SDKMAN! to return the current value of $JAVA_HOME."
  (let ((process-environment (list (format "SDKMAN_DIR=%s" sdkman-dir))))
    (with-temp-buffer
      (call-process "bash" nil t nil "-c"
                    "source $SDKMAN_DIR/bin/sdkman-init.sh; echo -n $JAVA_HOME")
      (buffer-string))))

(use-package eglot-java
  :load-path "lisp/eglot-java"
  :custom
  (eglot-java-file-new-ask-type nil)
  :config
  (setq eglot-java-java-home (sdkman-home)
        eglot-java-java-program (file-name-concat eglot-java-java-home
                                                  "bin/java"))
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

;; (defun indent-last-sexp ()
;;   (interactive)
;;   (let ((saved-point (point)))
;;     (save-excursion
;;       (backward-sexp)
;;       (indent-region (point) saved-point))))
;; bind M-; to comment-line?

(use-package ispell
  :config
  (setopt ispell-program-name "hunspell"
          ispell-dictionary "en_GB"))

(use-package flyspell
  :config
  (unbind-key [down-mouse-2] 'flyspell-mode-map)
  (unbind-key "C-M-i" 'flyspell-mode-map)
  :bind (:map flyspell-mode-map
	      ([mouse-3] . flyspell-correct-word)))

;; (use-package flymake-aspell
;;   :disabled t
;;   :hook (org-mode . flymake-aspell-setup))
