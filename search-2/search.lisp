;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.

(defpackage :jsearch
  (:use :common-lisp))

(in-package :jsearch)

;;
;; define a network 

(setf (get 's 'neighbors) '(a d)
      (get 'a 'neighbors) '(s b d)
      (get 'b 'neighbors) '(a c e)
      (get 'c 'neighbors) '(b)
      (get 'd 'neighbors) '(s a e)
      (get 'e 'neighbors) '(b d f)
      (get 'f 'neighbors) '(e))

;;
;; for the given path, check if there are any circular paths
(defun extend (path)
   (print (reverse path))
  (mapcar #'(lambda (new-node) (cons new-node path))
          (remove-if #'(lambda (neighbor) (member neighbor path))
                     (get (first path) 'neighbors))))


;;
;; walk the network, using depth-first search
(defun depth-first (start finish &optional
                              (queue (list (list start))))
  (cond((endp queue) nil)
       ((eq finish (first (first queue)))
        (reverse (first queue)))
       (t (depth-first
           start
           finish
           (append (extend (first queue))
                   (rest queue))))))


(defun breadth-first (start finish &optional
                              (queue (list (list start))))
  (cond((endp queue) nil)
       ((eq finish (first (first queue)))
        (reverse (first queue)))
       (t (breadth-first
           start
           finish
           (append (rest queue)
                   (extend (first queue))
)))))

