;;; lina-yaml.el --- lina-yaml  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Lina Bhaile <emacs-devel@linabee.uk>

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

(require 'project)
(eval-when-compile
  (require 'subr-x))

(defconst lina-yaml-abbrevs
  (eval-when-compile
    (let (abbrevs)
      (dolist (token '("apiVersion"
                       "kind"
                       "metadata"
                       "spec"
                       "HelmRelease"
                       "HelmRepository"
                       "GitRepository"
                       "namespace"
                       "Namespace"
                       "Kustomization"
                       "OCIRepository"
                       "ConfigMap"
                       "v1alpha1"))
        (push (list token token nil :system t) abbrevs))
      (dolist (abbrev '(("htfi2" "helm.toolkit.fluxcd.io/v2")
                        ("stfi1" "source.toolkit.fluxcd.io/v1")
                        ("ktfi1" "kustomize.toolkit.fluxcd.io/v1")))
        (push (append abbrev '(nil :system t)) abbrevs))
      abbrevs)))

(use-package autoinsert
  :ensure nil
  :config
  (setf (alist-get "\\.ya?ml\\'" auto-insert-alist nil nil #'equal)
        '(nil "---\n")))

(use-package indent-bars
  :pin gnu
  :custom ((indent-bars-display-on-blank-lines nil)
           (indent-bars-starting-column 0)
           (indent-bars-highlight-selection-method 'context)
           (indent-bars-color-by-depth nil)
           (indent-bars-highlight-current-depth (list :pattern indent-bars-pattern))
           (indent-bars-width-frac 0.25)
           (indent-bars-pad-frac 0.1)))

(defvar yaml-indent-offset 2)

(defun lina-yaml-indent ()
  (let (prev-ci in-list ci col)
    (save-excursion
      (forward-line -1)
      (setq prev-ci (current-indentation))
      (forward-char prev-ci)
      (setq in-list (looking-at (rx "-" space))))
    (cond
     ((eq this-command 'newline)
      (indent-line-to (+ prev-ci (if in-list yaml-indent-offset 0))))
     ((and (eq last-command this-command)
           (progn
             (setq ci (current-indentation))
             (/= ci 0)))
      (save-excursion
        (indent-line-to (* yaml-indent-offset (/ (- ci 1)
                                                 yaml-indent-offset)))))
     (t
      (save-excursion
        (indent-line-to (+ prev-ci (* yaml-indent-offset (if in-list 2 1)))))))
    (if (< (setq col (current-column))
           (setq ci (current-indentation)))
        (forward-char (- ci col)))))

(with-eval-after-load 'electric
  (cl-pushnew #'lina-yaml-indent electric-indent-functions-without-reindent))

(defun lina-yaml-meta-return ()
  (interactive)
  (newline)
  (indent-line-to (save-excursion
                    (search-backward-regexp "-")
                    (current-indentation)))
  (insert "- "))

(defun lina-yaml-electric-delete (&optional arg)
  (interactive "p")
  (cl-flet ((del ()
              (if (and (bound-and-true-p puni-mode)
                       (fboundp 'puni-backward-delete-char))
                  (puni-backward-delete-char arg)
                (call-interactively 'backward-delete-char))))
    (if (or (and arg
                 (> arg 1))
            (use-region-p))
        (del)
      (let ((col (current-column))
            (ci (current-indentation)))
        (if (and (not (bolp))
                 (<= col ci)
                 (= (mod ci yaml-indent-offset) 0))
            (indent-line-to (- ci yaml-indent-offset))
          (del))))))

(defun lina-yaml-hook ()
  (setopt-local tab-always-indent t
                yas-indent-line 'fixed
                indent-line-function #'lina-yaml-indent)
  (visual-line-mode -1)
  (electric-pair-local-mode -1)
  (whitespace-mode)
  (when (fboundp 'indent-bars-mode)
    (indent-bars-mode))
  (let ((auto-insert-query nil))
    (auto-insert)))

(use-package yaml-mode
  :disabled t
  :pin nongnu-devel
  :ensure nil
  :demand t
  :config
  (setq yaml-mode-abbrev-table nil)
  (define-abbrev-table 'yaml-mode-abbrev-table lina-yaml-abbrevs)
  :hook (yaml-mode-hook . lina-yaml-hook)
  :bind (:map yaml-mode-map
              ("DEL" . yaml-electric-backspace)))

(use-package yaml-ts-mode
  :ensure nil
  :functions (treesit--indent-rules-optimize
              treesit-simple-indent
              treesit-indent
              treesit-indent-region
              treesit-inspect-mode
              lina-yaml-ts-indent-line)
  :config
  (setq yaml-ts-mode-abbrev-table nil)
  (define-abbrev-table 'yaml-ts-mode-abbrev-table lina-yaml-abbrevs)
  (defun lina-yaml-ts-mode-hook ()
    (treesit-inspect-mode))
  :hook ((yaml-ts-mode-hook . lina-yaml-ts-mode-hook)
         (yaml-ts-mode-hook . lina-yaml-hook))
  :bind (:map yaml-ts-mode-map
              ("DEL" . nil)
              ("M-RET" . lina-yaml-meta-return))
  :mode ((rx ".y" (? "a") "ml" (? ".tpl") eos)
         "/\\.kube/kuberc\\'"))

(provide 'lina-yaml)
;;; lina-yaml.el ends here
