;; -*- lexical-binding: t; -*-
(load (setq custom-file (locate-user-emacs-file "custom.el")) t)

(add-to-list 'load-path (locate-user-emacs-file "lisp/") t)

(setq-default
 package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                    ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                    ("melpa" . "https://melpa.org/packages/"))
 package-install-upgrade-built-in t
 use-package-always-defer t)

(load "funs/autoloads")

(package-install 'delight)
(autoload 'setq-mode-local "mode-local")

(load "init/emacs")
(load "init/isearch")
(load "init/help")
(load "init/conf-mode")
(load "init/recentf.el")
(load "init/mwheel.el")
(load "init/icomplete.el")
(load "init/xref")
(load "init/project")
(load "init/compile")
(load "init/flymake")
(load "init/eglot")
(load "init/orderless")
(load "init/marginalia")
(load "init/consult")
(load "init/embark")
(load "init/corfu")
(load "init/cape")
(load "init/window")
(load "init/tab-line")
(load "init/gcmh")
(load "init/modus-themes")
(load "init/display-fill-column-indicator")
(load "init/display-line-numbers")
(load "init/paren")
(load "init/autorevert")
(load "init/prog-mode")
(load "init/elisp-mode")
(load "init/autoinsert")
(load "init/pp")
(load "init/make-mode")
;; (load "init/asm-mode")
(load "init/python")
(load "init/treesit")
(load "init/puni")
(load "init/yaml-mode")
(load "init/hcl-mode")
(load "init/nix-mode")
;; (load "init/nftables-mode")
(load "init/rust-mode")
;; (load "init/caddyfile-mode")
(load "init/aggressive-indent.el")
;; (load "init/reformatter")

(load "init/text-mode")
(load "init/ispell")
(load "init/flyspell")
(load "init/face-remap")
(load "init/org")
(load "init/auctex")
(load "init/markdown-mode")
(load "init/comint")
(load "init/ielm")
(load "init/shell")
(load "init/dired")
(load "init/tramp")
(load "init/man")
(load "init/finder")
(load "init/vterm")
(load "init/envrc")
(load "init/magit")
(load "init/faces")
(load "init/desktop")
(load "init/find-func")
(load "init/eshell")
(load "init/dockerfile-ts-mode")
(load "init/html-ts-mode")

(setopt ert-debug-on-error t)

(use-package devcontainer
  :preface
  (package-install-file (locate-user-emacs-file "lisp/devcontainer.el"))
  :custom
  (devcontainer-engine 'podman)
  (devcontainer-dotfiles-repository "https://github.com/lina-bh/dotfiles.git"))

(use-package prettier-js
  :commands prettier-js)

(use-package astro-ts-mode
  :ensure t
  :config
  (defun lina-astro-hook ()
    (display-line-numbers-mode))
  :hook (astro-ts-mode . lina-astro-hook)
  :mode "\\.astro\\'")

(use-package css-mode
  :bind (:map css-mode-map
              ("C-c C-c" . recompile)))
