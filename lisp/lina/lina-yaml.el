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

(require 'yaml-mode)
(require 'yaml)

(use-package yaml-mode
  :pin nongnu-devel
  :ensure t
  :defines auto-insert-alist auto-insert-query
  :init
  (with-eval-after-load 'autoinsert
    (setf (alist-get "\\.ya?ml\\'" auto-insert-alist nil nil #'equal)
          '(nil "---\n")))
  :config
  (setq yaml-mode-abbrev-table nil)
  (define-abbrev-table 'yaml-mode-abbrev-table
    (eval-and-compile
      (let (abbrevs)
        (dolist (token '("apiVersion"
                         "kind"
                         "metadata"
                         "spec"
                         "HelmRelease"
                         "HelmRepository"
                         "GitRepository"
                         "Namespace"
                         "Kustomization"))
          (push (list token token nil :system t) abbrevs))
        (dolist (abbrev '(("htfi2" "helm.toolkit.fluxcd.io/v2")
                          ("stfi1" "source.toolkit.fluxcd.io/v1")))
          (push (append abbrev '(nil :system t)) abbrevs))
        (dolist (skeleton
                 '(("k8s" . skeleton-k8s)
                   ("ns" . skeleton-k8s-namespace)
                   ("gr" . skeleton-k8s-gitrepository)
                   ("hrel" . skeleton-k8s-helmrelease)
                   ("fk" . skeleton-k8s-kustomization.kustomize.toolkit.fluxcd.io)
                   ("ocir" . skeleton-k8s-ocirepository)))
          (push (list (car skeleton) "" (cdr skeleton) :system t) abbrevs))
        abbrevs)))
  (defun lina-yaml-hook ()
    (setopt-local tab-always-indent t
                  whitespace-style '( face tabs spaces trailing
                                      space-before-tab indentation
                                      empty space-after-tab tab-mark
                                      missing-newline-at-eof))
    (visual-line-mode -1)
    (whitespace-mode)
    (let ((auto-insert-query nil))
      (auto-insert)))
  :hook (yaml-mode-hook . lina-yaml-hook)
  :bind (:map yaml-mode-map
              ("DEL" . backward-delete-char-untabify)
              ("C-w" . backward-kill-sexp)))

(use-package yaml-ts-mode
  :ensure nil
  :preface
  (defvar-local lina-yaml-ts-indent-extra nil)
  :functions (treesit--indent-rules-optimize
              treesit-simple-indent
              treesit-indent
              treesit-indent-region
              treesit-inspect-mode
              lina-yaml-ts-indent-line)
  :config
  (defun lina-yaml-ts-indent-line ()
    (if (eq last-command this-command)
        (progn
          (back-to-indentation)
          (let ((tab-stop-list (list 0 yaml-indent-offset)))
            (tab-to-tab-stop))
          (forward-to-indentation 0))
      (treesit-indent)))
  (defun lina-yaml-ts-mode-hook ()
    (setq-local tab-always-indent t
                treesit-simple-indent-rules
                (treesit--indent-rules-optimize
                 ;; ACHTUNG: rules were written by gpt-6-astra
                 `((yaml
                    ((node-is ,(rx bos (or "}" "]") eos))
                     parent-bol 0)
                    ((parent-is ,(rx bos "flow_" (or "mapping" "sequence") eos))
                     parent-bol yaml-indent-offset)
                    ((query "(block_mapping_pair value: (block_node (block_sequence)) @indent)")
                     parent-bol 0)
                    ((parent-is "block_sequence") first-sibling 0)
                    ((parent-is "block_mapping") first-sibling 0)
                    ((match "block_node" "block_mapping_pair" "value")
                     parent-bol yaml-indent-offset)
                    ((parent-is "block_sequence_item")
                     parent-bol yaml-indent-offset))))
                treesit-indent-function #'treesit-simple-indent
                indent-region-function #'treesit-indent-region
                indent-line-function #'lina-yaml-ts-indent-line)
    (treesit-inspect-mode))
  :hook (yaml-ts-mode-hook . lina-yaml-ts-mode-hook)
  :bind (:map yaml-ts-mode-map
              ("DEL" . backward-delete-char-untabify)))

(provide 'lina-yaml)
;;; lina-yaml.el ends here
