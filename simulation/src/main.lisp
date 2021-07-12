(defpackage simulation
  (:use :cl))
(in-package :simulation)

;; blah blah blah.
(defun tt (name)
  (format t "hello ~A" name))

(defvar *start* nil)

(defstruct (event (:print-function print-event )) 
  (time 'unknown)
  (input-task nil)
  (output-tasks nil))

(defun print-event (structure @rest ignore)
  (format t "<event structure>"))

(defstruct (task (:print-function print-task))
  (name 'unknown)
  (input-events nil)
  (output-event nil)
  (duration 'unknown))

(defun print-task (structure @rest ignore)
  (format t "<task structure for ~a>" (task-name structure)))

(defun connect (task output inputs)
  (setf (task-input-events task) inputs)
  (dolist (event inputs)
    (push task (event-output-tasks event)))
  (setf (task-output-event task) output)
  (setf (event-input-task output) task))

(let ((s (make-task :duration 4 :name 'select))
      (n (make-task :duration 6 :name 'negotiate))
      (d (make-task :duration 4 :name 'design))
      (p (make-task :duration 7 :name 'prepare))
      (f (make-task :duration 8 :name 'furnish))
      (m (make-task :duration 1 :name 'move))
      (e1 (make-event :time 0))
      (e2 (make-event))
      (e3 (make-event))
      (e4 (make-event))
      (e5 (make-event))
      (e6 (make-event))
      (e7 (make-event)))
  (setf *start* e1)
  (connect s e2 (list e1))
  (connect n e3 (list e2))
  (connect d e4 (list e2))
  (connect p e5 (list e3 e4))
  (connect f e6 (list e3 e4))
  (connect m e7 (list e5 e6))
  'done)

(defun simulate-event (time event)
  (setf (event-time event) time)
  (dolist (output-task (event-output-tasks event))
    (simulate-task output-task)))

(defun event-times-known-p (list-of-events)
  (not (find-if-not
        #'(lambda (event) (numberp (event-time event)))
        list-of-events)))

(defun lastest-time (events)
  (apply #'max (mapcar #'event-time events)))

(defun announce-start-time (time name)
  (format t "~%Time ~a:~9tstarting~19t~a." time name))

(defun announce-finish-time (time name)
  (format t "~%Time ~a:~9tfinishing~19t~a." time name))

(defun simulate-task (task)
  (when (event-times-known-p (task-input-events task))
    (let* ((start-time (lastest-time (task-input-events task)))
           (finish-time (+ (task-duration task) start-time)))
      (announce-start-time start-time (task-name task))
      (announce-finish-time finish-time (task-name task))
      (simulate-event finish-time (task-output-event task)))
    ))


;;
;; how to run this
;; (simulate-event 0 *start)
