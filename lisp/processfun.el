;; -*- lexical-binding: t; -*-
(eval-when-compile
  (require 'cl-lib))
(require 'generator)

;; reminder to self: figure out how to put a bound on the queue

(cl-defstruct (funprocess (:constructor funprocess)
                          (:copier nil))
  process stderr queue iter)

(defun processfun--push (element funprocess)
  (push element (funprocess-queue funprocess)))

;; so i can fucking remember this
;; if the list has one element return it and make the list empty
;; otherwise search for the first cons where the cdr of its cdr is nil
;; so the car is the second to last element.
;; return the car of the cdr and set cdr to nil.

(defun processfun--dequeue (fp)
  (let ((queue (funprocess-queue fp)))
    (cl-labels ((f (queue)
                  (cond ((null (cdr queue))
                         (prog1
                             (car queue)
                           (setf (funprocess-queue fp) nil)))
                        ((cddr queue)
                         (f (cdr queue)))
                        (t
                         (prog1
                             (cadr queue)
                           (setcdr queue nil))))))
      (f queue))))

(defun processfun--stderr-process (exec fp)
  (let ((filter (lambda (_ stderr)
                  (processfun--push (list 'stderr stderr) fp))))
    (make-pipe-process :name (format "%s-stderr" exec)
                       :buffer nil
                       :noquery t
                       :filter filter
                       :sentinel #'ignore)))

(defun processfun--process (args funprocess)
  (let ((filter (lambda (_ stdout)
                  (processfun--push (list 'stdout stdout) funprocess)))
        (sentinel (lambda (_ event)
                    (processfun--push (list 'exit (substring event 0 -1)) funprocess)))
        (stderr (processfun--make-stderr-process (car args) funprocess)))
    (cons (make-process :name (car args)
                        :buffer nil
                        :command args
                        :connection-type 'pipe
                        :filter filter
                        :sentinel sentinel
                        :stderr stderr)
          stderr)))

(defun processfun-start (args)
  (let ((funprocess (processfun-funprocess)))
    (cl-destructuring-bind (process . stderr)
        (processfun--make-process args funprocess)
      (setf (processfun-funprocess-process funprocess) process
            (processfun-funprocess-stderr funprocess) stderr))
    funprocess))

(defun start-funprocess (args)
  )
