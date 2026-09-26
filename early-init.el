;;; early-init.el --- early-init  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Lina Bhaile <emacs-devel@linabee.uk>
;; Copyright (c) 2014-2026 Henrik Lissner

;; Author: Lina Bhaile <emacs-devel@linabee.uk>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(setq default-frame-alist '((width . 180) (height . 36))
      initial-frame-alist (append default-frame-alist
                                  '((fullscreen . maximized)))
      recentf-auto-cleanup 'never
      recentf-keep nil
      gc-cons-threshold most-positive-fixnum
      vc-handled-backends '(Git)
      load-prefer-newer t)

(unless (memq window-system '(w32))
  (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; doomemacs/core@2e0dbaddd71600392498428c4b0e827c72a81e0e early-init.el:51
;; "Performance on Windows is considerably worse than elsewhere. We'll need
;; everything we can get."
(when (boundp 'w32-get-true-file-attributes)
  (setq w32-get-true-file-attributes nil    ; reduce IO ops
        w32-pipe-read-delay 0               ; faster IPC
        w32-pipe-buffer-size (* 64 1024)))  ; read more at a time (was 4K)

;;; early-init.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
