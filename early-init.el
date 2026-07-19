;; -*- lexical-binding: t; -*-
(setq initial-frame-alist (append default-frame-alist '((fullscreen . maximized)))
      recentf-auto-cleanup 'never
      recentf-keep nil
      gc-cons-threshold most-positive-fixnum
      vc-handled-backends '(Git)
      load-prefer-newer t)
(menu-bar-mode -1)
;; (autoload 'tool-bar-mode "tool-bar.el")
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
