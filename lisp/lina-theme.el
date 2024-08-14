;; -*- lexical-binding: t; -*-
(use-package modus-themes
  :demand t
  :load-path (lambda () (expand-file-name "./themes" data-directory))
  :custom
  (modus-themes-variable-pitch-ui t)
  (modus-themes-mixed-fonts t)
  (modus-themes-prompts '(intense))
  (modus-themes-to-toggle '(modus-operandi modus-vivendi))
  (modus-themes-common-palette-overrides '((fringe unspecified)
                                           (bg-line-number-inactive unspecified)
                                           (bg-line-number-active unspecified)
                                           (fg-line-number-active fg-main)))
  :config
  (defun my-modus-custom ()
    (modus-themes-with-colors
      (custom-set-faces
       `(mode-line ((,c (:inherit nil))))
       `(mode-line-inactive ((,c (:inherit nil))))
       `(tab-line-highlight ((,c (:inherit nil)))))))
  ;; obsolete in emacs 30
  ;; (setq modus-themes-fringes nil
  ;;       modus-themes-mode-line '(3d)
  ;;       modus-themes-subtle-line-numbers t
  ;;       modus-themes-syntax '(green-strings))
  (add-hook 'modus-themes-after-load-theme-hook
            #'my-modus-custom)
  (unless (memq 'modus-operandi custom-enabled-themes)
    (load-theme 'modus-operandi t)))
;; :bind ("<f5>" . modus-themes-toggle)
