;; -*- lexical-binding: t; -*-
;; (add-to-list 'load-path (expand-file-name "./themes" data-directory))
;; (autoload 'modus-themes-with-colors "modus-themes" nil nil 'macro)

;; (use-package modus-themes
;;   :ensure t)

(setopt modus-themes-variable-pitch-ui t
        modus-themes-mixed-fonts t
        modus-themes-prompts '(intense)
        modus-themes-to-toggle '(modus-operandi modus-vivendi)
        modus-themes-common-palette-overrides
        '((fringe unspecified)
          (bg-line-number-inactive unspecified)
          (bg-line-number-active unspecified)
          (fg-line-number-active fg-main)))

(add-hook 'modus-themes-after-load-theme-hook
          (defun my-modus-custom ()
            (modus-themes-with-colors
              (custom-set-faces
               `(mode-line ((,c (:inherit nil))))
               `(mode-line-inactive ((,c (:inherit nil))))
               `(tab-line-highlight ((,c (:inherit nil))))))))

(add-hook 'after-init-hook (lambda ()
                             (load-theme 'modus-operandi t)))

(provide 'my-theme)
