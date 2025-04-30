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
  (project-prompter #'my-project-prompt-dir)
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
  (defun find-flake ()
    (interactive)
    (let ((default-directory (project-root (project-current t))))
      (find-file "./flake.nix")))
  :bind
  ("M-!" . project-async-shell-command)
  ("M-&" . project-async-shell-command)
  ;; ("C-c C-c" . project-compile)
  ("C-x p b" . project-list-buffers)
  ("C-x p d" . project-dired))
