;; -*- lexical-binding: t; -*-
(setq default-frame-alist `((inhibit-default-buffering
                             .
                             ,(eq initial-window-system 'pgtk))
                            (background-mode . ,(if (display-graphic-p)
                                                    'light
                                                  'dark)))
      initial-frame-alist (append default-frame-alist '((fullscreen . maximized)))
      recentf-auto-cleanup 'never
      recentf-keep nil
      gc-cons-threshold most-positive-fixnum
      vc-handled-backends nil
      load-prefer-newer t)
;; (autoload 'tool-bar-mode "tool-bar.el")
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
