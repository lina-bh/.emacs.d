(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(ergoemacs-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Consolas"))))
 '(variable-pitch ((t (:height 110 :family "Calibri")))))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(dolist (package package-selected-packages)
  (unless (package-installed-p package)
    (package-install package)))

(ido-mode t)
(show-paren-mode t)
(auto-save-visited-mode t)
(global-undo-tree-mode t)

;; scroll one line at a time (less "jumpy" than defaults)    
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse 't
      scroll-step 1) ;; keyboard scroll one line at a time

(setq create-lockfiles nil
      make-backup-files nil)

(setq-default default-directory (getenv "HOME"))

(setq org-adapt-indentation nil
      org-descriptive-links nil)

(add-hook 'prog-mode-hook (defun my-prog-mode-hook ()
			    (electric-pair-local-mode t)))

(add-hook 'text-mode-hook (defun my-text-mode-hook ()
			    (visual-line-mode t)
			    (variable-pitch-mode t)))

(defun indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max))))

(if (require 'ergoemacs-mode nil t)
    (progn
      (setq ergoemacs-keyboard-layout "gb")
      ;;  ergoemacs-theme "lvl3")
      (ergo)
      (ergoemacs-mode t))
  (cua-mode t))
