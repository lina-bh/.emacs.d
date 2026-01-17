;; -*- lexical-binding: t; -*-
(use-package modus-themes
  :ensure t
  :custom
  (modus-themes-variable-pitch-ui t)
  (modus-themes-mixed-fonts t)
  (modus-themes-prompts '(intense))
  (modus-themes-to-toggle '(modus-operandi modus-vivendi))
  (modus-themes-common-palette-overrides
   '((fringe unspecified)
     (bg-line-number-inactive unspecified)
     (bg-line-number-active unspecified)
     (fg-line-number-active fg-main)))
  :custom-face
  (modus-themes-ui-variable-pitch ((((type x pgtk))
                                    :family
                                    "Inter"
                                    :height
                                    102)))
  :config
  (defun my-modus-custom ()
    (modus-themes-with-colors
      (custom-set-faces
       `(mode-line ((,c (:inherit nil))))
       `(mode-line-inactive ((,c (:inherit nil))))
       `(tab-line-highlight ((,c (:inherit nil)))))))
  (defun my-load-modus ()
    (when (display-graphic-p)
      (load-theme 'modus-operandi t)))
  :hook (modus-themes-after-load-theme . my-modus-custom)
  :hook (after-init . my-load-modus))
