;; -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;; no time for commentary!
(require 'cl-lib)
(require 'seq)
(require 'tramp-container)

(defgroup devcontainer nil "Devcontainer support via Tramp.")

(defcustom devcontainer-docker-path '(podman . "podman")
  "Path or name of Docker CLI-compatible (+Podman) tool.

You either want something like \\='(podman . \"podman\") or \\='(docker . \"docker\").

The behaviour of `devcontainer--find-container-workspace' changes if you
switch this between Docker and Podman because Tramp implements different
methods for both."
  ;; FIXME As of 2025-04-18, only \"podman\" is supported."
  :type '(choice (cons (const 'podman) string)
                 (cons (const 'docker) string))
  :group 'devcontainer)

(defcustom devcontainer-engine "podman"
  "The container engine to use, one of \"podman\" or \"docker\".

To specify the path or name of the podman/docker executable, customise
`tramp-podman-program' or `tramp-docker-program'."
  :group 'devcontainer
  :type '(choice (const "podman")
                 (const "docker")))

(defcustom devcontainer-dotfiles-repository nil
  "--dotfiles-repository argument to devcontainer CLI.

If non-nil, this argument is passed to devcontainer which should clone
and install your dotfiles inside the container. See
`devcontainer--up-build-command'."
  :type '(choice (const nil)
                 string)
  :group 'devcontainer)

(defcustom devcontainer-cli '("devcontainer")
  "Initial elements of any devcontainer command arguments.

If you need to specify where your Node.js is, put that before the
absolute path to \"devcontainer\"."
  :type '(list string)
  :group 'devcontainer)

(cl-defun devcontainer--find-container-workspace (
                                                  &key host
                                                  &key user
                                                  &key dir
                                                  )
  "Given the parts required to find the workspace directory mounted inside
the newly created container, call `dired' to edit that directory."
  (dired (format "/%s:%s@%s:%s"
                 devcontainer-engine
                 user
                 host
                 dir)))

(defun devcontainer--up-stdout-filter (proc out)
  "When \"devcontainer up\" returns, parse the JSON object we get from stdout
and call `devcontainer--find-container-workspace' with the extracted fields.

Errors if the object's \"outcome\" field doesn't equal \"success\"."
  ;; Plenty of bugs and missing checks here as it is.
  (let ((object (json-parse-string out :object-type 'plist)))
    (if (string-equal "success" (plist-get object :outcome))
        (devcontainer--find-container-workspace
         :user (plist-get object :remoteUser)
         :host (plist-get object :containerId)
         :dir (plist-get object :remoteWorkspaceFolder))
      (error "outcome != \"success\": %S" object))))

(defun devcontainer--workspace-folder ()
  (let ((path (if current-prefix-arg
                  (read-directory-name "Directory with .devcontainer.json: ")
                (or buffer-file-name
                    dired-directory
                    (error "Buffer must be attached to a file or directory")))))
    (if (file-remote-p path)
        ;; FIXME is this worth doing if its even possible
        ;; can do something like "/ssh:user@host|podman:vscode@ctr/"
        ;; but i can tell it involves pain.
        (error "Will not create devcontainers for remote connections"))
    (expand-file-name
     (or
      (locate-dominating-file path ".devcontainer/devcontainer.json")
      (locate-dominating-file path ".devcontainer.json")
      (error "Could not locate devcontainer.json")))))

(defun devcontainer--docker-path ()
  (if (string= "docker" devcontainer-engine)
      tramp-docker-program
    tramp-podman-program))

(defun devcontainer--up-build-command (workspace-folder)
  (let ((command (append
                  devcontainer-cli
                  (list
                   "up"
                   (format "--docker-path=%s" (devcontainer--docker-path))
                   (format "--workspace-folder=%s" workspace-folder)))))
    (if devcontainer-dotfiles-repository
        (setq command (append command
                              (list
                               (format "--dotfiles-repository=%s"
                                       devcontainer-dotfiles-repository)))))
    command))

(defun devcontainer--buffer (subcmd label)
  (let ((buffer (get-buffer-create
                 (format "*devcontainer %s %s*" subcmd label))))
    (with-current-buffer buffer
      (run-mode-hooks))
    buffer))

(defun devcontainer-up ()
  "Bring up a devcontainer."
  (interactive)
  (let* ((workspace-folder (devcontainer--workspace-folder))
         (command (devcontainer--up-build-command workspace-folder))
         (buffer (devcontainer--buffer "up" workspace-folder)))
    (with-current-buffer buffer
      (fundamental-mode)
      (erase-buffer)
      (shell-command-mode))
    (display-buffer buffer)
    (make-process
     :name (string-join command " ")
     :buffer nil
     :filter #'devcontainer--up-stdout-filter
     :stderr buffer
     :command command
     :connection-type 'pipe)))

(defun devcontainer-down ()
  "Save and kill buffers opened inside the devcontainer and remove it."
  (interactive)
  (when-let* ((ident (file-remote-p (or buffer-file-name
                                        dired-directory
                                        default-directory)
                                    t))
              (vec (tramp-dissect-file-name ident)))
    (tramp-cleanup-connection vec)
    (call-process (devcontainer--docker-path)
                  nil           ; INFILE
                  (messages-buffer)
                  nil           ; DISPLAY
                  "rm" "-f" (tramp-file-name-host vec))))

(provide 'devcontainer)
