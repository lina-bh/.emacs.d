;; -*- no-byte-compile: t; -*-
(setq load-prefer-newer t
      gc-cons-threshold most-positive-fixnum
      default-frame-alist '((width . 100))
      initial-frame-alist (cons '(fullscreen . maximized) default-frame-alist))
(menu-bar-mode -1)
(tool-bar-mode -1)
(set-scroll-bar-mode 'right)
