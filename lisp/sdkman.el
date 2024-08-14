;; -*- lexical-binding: t; -*-
(defvar sdkman-dir (expand-file-name "~/.sdkman")
  "Path to SDKMAN!, a version manager for Java virtual machines and tools.")

(defun sdkman-java-home ()
  "Call SDKMAN! to return the current value of $JAVA_HOME."
  (let ((process-environment (list (format "SDKMAN_DIR=%s" sdkman-dir))))
    (with-temp-buffer
      (call-process "bash" nil t nil "-c"
                    "source $SDKMAN_DIR/bin/sdkman-init.sh; echo -n $JAVA_HOME")
      (buffer-string))))

(defun sdkman-paths ()
  "Call SDKMAN! and find out what it is setting the path to."
  (let (env-path
        (process-environment (list (format "SDKMAN_DIR=%s" sdkman-dir))))
    (with-temp-buffer
      (call-process "bash" nil t nil "-c"
                    "source $SDKMAN_DIR/bin/sdkman-init.sh; echo -n $PATH")
      (setq env-path (buffer-string))
      (seq-filter (lambda (x)
                    (string-match-p "\\.sdkman/" x))
                  (string-split env-path ":")))))

(provide 'sdkman)
