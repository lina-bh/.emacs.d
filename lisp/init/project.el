;; -*- lexical-binding: t; -*-
(use-package project
  :ensure nil
  :autoload project-root
  :functions my-project-prompt-dir
  :commands project-add-dir-local-variable find-flake
  :custom
  (project-vc-extra-root-markers '(".git"
                                   ".project"
                                   "pom.xml"
                                   "Cargo.toml"
                                   "flake.nix"
                                   ".devcontainer.json"
                                   ".devcontainer/"))
  (project-switch-use-entire-map t)
  :config
  (defun my-project-prompt-dir ()
    (if current-prefix-arg
        (read-directory-name "Select directory: " nil nil t)
      (or
       (and-let* ((project (project-current))) (project-root project))
       default-directory)))
  (defun project-add-dir-local-variable ()
    (interactive)
    (let ((default-directory (project-root (project-current))))
      (call-interactively #'add-dir-local-variable)))
  :bind
  ("C-x p d" . project-dired)
  ("C-x p s" . project-eshell))
