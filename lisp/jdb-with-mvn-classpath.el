;; -*- lexical-binding: t; -*-
(defconst jdb-with-mvn-classpath-buffer-name "*jdb maven*")

(defun jdb-with-mvn-classpath--maven ()
  (seq-find (lambda (args) (executable-find (car args)))
            '(("mvnd" "--raw-streams") ("mvn"))))

(defun jdb-with-mvn-classpath--sentinel (process event)
  (if (not (string= event "finished\n"))
      (message "%s %s, see %s"
               (process-name process)
               (substring event 0 -1)
               jdb-with-mvn-classpath-buffer-name)
    (let ((classpath-arg (with-current-buffer buffer (string-trim
                                                      (buffer-string)))))
      (funcall-interactively #'jdb (format "jdb -classpath%s %s"
                                           classpath-arg
                                           command-line)))))

(defun jdb-with-mvn-classpath (command-line)
  (interactive
   `(,(read-string "jdb args: ")))
  (let ((default-directory (project-root (project-current)))
        (buffer (get-buffer-create jdb-with-mvn-classpath-buffer-name))
        (args `(,@(jdb-with-mvn-classpath--maven) "-q" "exec:exec"
                "-Dexec.executable=echo" "-Dexec.args=%classpath")))
    (with-current-buffer buffer
      (erase-buffer))
    (make-process :name (car args)
                  :command args
                  :connection-type 'pipe
                  :buffer buffer
                  :sentinel #'jdb-with-mvn-classpath--sentinel)))

(provide 'jdb-with-mvn-classpath)
