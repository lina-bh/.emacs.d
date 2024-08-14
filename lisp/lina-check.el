;; -*- lexical-binding: t; -*-
(use-package project
  :functions my-project-prompt-dir
  :custom
  (project-prompter #'my-project-prompt-dir)
  (project-vc-extra-root-markers '(".project" "pom.xml" "Cargo.toml"))
  (vc-handled-backends '(Git))
  :config
  (defun project-add-dir-local-variable ()
    (interactive)
    (let ((default-directory (project-root (project-current))))
      (call-interactively #'add-dir-local-variable)))
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
  :bind
  ([remap shell-command] . project-shell-command)
  ([remap async-shell-command] . project-async-shell-command))

(use-package eglot
  :custom
  (eglot-ignored-server-capabilities '(:inlayHintProvider))
  (eglot-report-progress nil)
  (eglot-events-buffer-config '(:size nil :format full))
  :config
  (defun my-eglot-hook ()
    (setq-local eldoc-echo-area-use-multiline-p (eglot-managed-p)))
  (setf (alist-get 'web-mode eglot-server-programs)
        (alist-get 'html-mode eglot-server-programs))
  :hook (eglot-managed-mode . my-eglot-hook)
  :bind (:map eglot-mode-map
              ("<f2>" . eglot-rename)
	      ("C-c 2" . eglot-rename)))

(defun indent-region-or-buffer (beg end)
  "Call `indent-region' on either the selected region or the whole buffer."
  (interactive "r")
  (unless (use-region-p)
    (setq beg (point-min)
          end (point-max)))
  (save-excursion
    (indent-region beg end)
    (whitespace-cleanup-region beg end)))

(bind-key "C-c f" #'indent-region-or-buffer)

(use-package ispell
  :config
  (setopt ispell-program-name "hunspell"
          ispell-dictionary "en_GB"))

(use-package flyspell
  :config
  (unbind-key [down-mouse-2] 'flyspell-mode-map)
  (unbind-key "C-M-i" 'flyspell-mode-map)
  :bind (:map flyspell-mode-map
	      ([mouse-3] . flyspell-correct-word)))

(provide 'lina-check)
