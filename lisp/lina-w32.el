;; -*- lexical-binding: t; -*-
(use-package emacs
  :if (eq system-type 'windows-nt)
  :custom
  ;; https://github.com/doomemacs/doomemacs/blob/master/lisp/doom-start.el#L85
  (w32-get-true-file-attributes nil)
  (w32-pipe-read-delay 0)
  (w32-pipe-buffer-size 65536)
  :custom-face
  (default ((((type w32)) (:family "Consolas" :height 95))))
  (fixed-pitch ((((type w32)) (:family "Consolas" :height 95))))
  (variable-pitch ((((type w32)) (:family "Tahoma" :height 100))))
  (fill-column-indicator ((((type w32)) (:family "SimSun" :foreground "pink"
                                                :background unspecified))))
  (tab-line ((((type w32)) (:family "Arial" :height 100)))))

;; last resort for flyspell: use aspell inside WSL
;; (eval-after-load 'ispell
;;   (setq ispell-program-name (expand-file-name
;; 			     "aspell.cmd"
;; 			     user-emacs-directory)))
