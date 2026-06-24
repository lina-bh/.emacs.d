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
(when (string-equal "framework" (system-name))
  (setq native-comp-compiler-options '("-O3"
                                       "-march=znver4"
                                       "-fno-plt"
                                       "-fstack-clash-protection"
                                       "-fcf-protection"
				       "-fno-omit-frame-pointer"
                                       "-fno-finite-math-only")
        native-comp-driver-options '("-pipe"
				     "-Wp,D_FORTIFY_SOURCE=3"
                                     "-Wl,-O2"
                                     "-Wl,--sort-common"
                                     "-Wl,--as-needed"
                                     "-Wl,-z,relro"
                                     "-Wl,-z,now"
                                     "-Wl,-z,pack-relative-relocs")))
(menu-bar-mode -1)
;; (autoload 'tool-bar-mode "tool-bar.el")
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
