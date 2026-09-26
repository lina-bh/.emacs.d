;;; lina-fmt.el --- lina-fmt  -*- lexical-binding: t; -*-

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

(autoload 'setq-mode-local "mode-local" nil nil t)

(use-package apheleia
  :defines apheleia-mode-alist python-mode
  :ensure t
  :pin melpa
  :custom
  (apheleia-global-mode t)
  (apheleia-inhibit-functions (list (lambda ()
                                      (null buffer-file-name))))
  (apheleia-formatters
   '((ruff "uvx"
           "--quiet"
           "ruff"
           "format"
           "--silent"
           (apheleia-formatters-fill-column "--line-length")
           "--stdin-filename"
           filepath
           "-")
     (ruff-isort "uvx"
                 "--quiet"
                 "ruff"
                 "check"
                 "-n"
                 "--select"
                 "I"
                 "--fix"
                 "--fix-only"
                 "--stdin-filename"
                 filepath
                 "-")
     (tex-fmt "tex-fmt"
              "--stdin"
              "-v")
     (jq "jq"
         "."
         "-M"
         (apheleia-formatters-indent "--tab" "--indent"))
     (prettier-javascript "apheleia-npx"
                          "prettier"
                          "--stdin-filepath"
                          filepath
                          "--parser=babel-flow"
                          (apheleia-formatters-js-indent "--use-tabs"
                                                         "--tab-width"))
     (prettier-typescript "apheleia-npx"
                          "prettier"
                          "--stdin-filepath"
                          filepath
                          "--parser=typescript"
                          (apheleia-formatters-js-indent "--use-tabs"
                                                         "--tab-width"))
     (biome "apheleia-npx" "biome" "check" "--write" "--linter-enabled=false"
            "--stdin-file-path" filepath)
     (opentofu "tofu" "fmt" "-")
     (yamlfmt "yamlfmt" "-in")))
  (apheleia-mode-alist `((python-mode . (ruff ruff-isort))
                         (,(rx ".tex" eos) . tex-fmt)
                         ,@(mapcar (lambda (mode)
                                     (cons mode 'jq))
                                   '(json-mode
                                     json-ts-mode))
                         (,(rx ".js" eos) . biome)
                         (,(rx ".ts" (? "x") eos) . biome)
                         (terraform-mode . opentofu)
                         (,(rx ".yaml" eos) . yamlfmt)))
  :config
  (setq-mode-local python-mode apheleia-formatters-respect-fill-column t))

(provide 'lina-fmt)
;;; lina-fmt.el ends here
