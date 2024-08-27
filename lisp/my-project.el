;; -*- lexical-binding: t; -*-
(autoload 'project-root "project")

(defun my-project-prompt-dir ()
  (let* (history-add-new-input
         (dir-choice "... (choose a dir)")
         (known-roots (project-known-project-roots))
         (pr-dir (completing-read "Select project: "
                                  (append
                                   (list default-directory)
                                   known-roots
                                   (list dir-choice))
                                  nil t nil 'known-roots)))
    (cond
     ((string-empty-p pr-dir) default-directory)
     ((string-equal pr-dir dir-choice)
      (read-directory-name "Select directory: " default-directory nil t))
     (t
      pr-dir))))

(defun project-add-dir-local-variable ()
  (interactive)
  (let ((default-directory (project-root (project-current))))
    (call-interactively #'add-dir-local-variable)))

(setopt project-prompter #'my-project-prompt-dir
        project-vc-extra-root-markers '(".project" "pom.xml" "Cargo.toml"))

(provide 'my-project)
