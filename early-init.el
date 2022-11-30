(setq load-prefer-newer t
      gc-cons-threshold most-positive-fixnum)
(tool-bar-mode 0) ;; prevent flash
(if (eq system-type 'darwin)
    (progn
      (add-to-list 'load-path
		   (locate-user-emacs-file "site-lisp/exec-path-from-shell"))
      (require 'exec-path-from-shell)
      (setq exec-path-from-shell-arguments '("-l"))
      (exec-path-from-shell-initialize))
  (menu-bar-mode 0))

