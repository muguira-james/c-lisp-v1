
;;
;; from the artificial intelligence book

;; create a depth and breadth first search
;;
;; packaging
(defpackage #:lrn
    (:use  #:cl 
     ))


(in-package #:lrn)


;;
;; This uses my own queue. It is  a simple list.  I append to the end with a (setf que (append ...)) pattern.
;; and I add to the front with list push.
;;
;; I use 2 queues: que, which holds my location in the search; visit, which remembers where i've been
;;
;; This could be improved:
;;  - use a package - tbd
;;  - generalize the queue functions to just have a  single function that works on both queues.
;;
;; =============================== handle the queue coding ==========================
;; hold location in the map
(defvar que nil)

;;
;; these are the nodes i've visited over the course of my search
(defvar visit nil)

;;
;; add a node to the end of the queue
(defun q-append (node-name)
  (setf que (append que (list node-name))))

;;
;; add a node to the front of a queue
(defun q-push (node-name)
  (push node-name que))

;;
;; remove the first node in the queue
(defun q-pop ()
  (pop que))

;;
;; just print the queue
(defun print-q ()
  que)

;;
;; does the queue contain the named node?
(defun q-contains (node-name)
  (numberp (position node-name que)))

;;
;; append a node to the end of the visit queue
(defun v-add (node-name)
  (setf visit (append visit (list node-name))))

;;
;; remove a node from the front of the visit queue
(defun v-pop ()
  (pop visit))


;;
;; test function 
(defun main ()
  (format t "learning lisp..."))

;;
;; =============================== handle the hash and nodes in the hash ============================
;;
;; define a network hash for the program
(defvar *hash*)

;;
;; create it as a hash
(setf *hash* (make-hash-table))

;;
;; Each node in the hash contains a city and the left / right children
;;
;; This class defines the left and right children and accessors
(defclass node ()
  ((left-child :initarg :left :accessor left)
   (right-child :initarg :right :accessor right)))

;;
;; show the contents of the hash, but this does not show node classes pretty
(defun show-raw-hash (hash)
  (maphash (lambda (key val) (format t "~%key= ~a : value= ~a" key val)) hash))

;;
;; dump the hash to the console
(defun print-hash-nodes (hash)
  (maphash (lambda (key val)
             (let ((left-child (left (gethash key hash)))
                   (right-child (right (gethash key hash))))
               (format t "~%Node name= ~a : left-child= ~a - right-child= ~a" key left-child right-child))) hash))

;;
;; show the node children in a pretty way
(defun show-children (node hash)
  (let ((left-child (left (gethash node hash)))
        (right-child (right (gethash node hash))))
    (format t "~%key= ~a : Left= ~a - Right= ~a" node left-child right-child))
  )

;;
;; return the left child of a  node
(defun get-left-child (node hash)
  (left (gethash node hash)))

;; return the right child of a node
(defun get-right-child (node hash)
  (right (gethash node hash)))

;;
;; add input node with children to named hash
;;
;; example (add-node-to-hash 'd 'e 'f *hash*)
;;
(defun add-node-to-hash (name left-child right-child hash)
  (setf (gethash name hash) (make-instance 'node :left left-child :right right-child)))


;;
;; build an example network

(add-node-to-hash 's 'a 'd *hash*)
(add-node-to-hash 'a 'b 'd *hash*)
(add-node-to-hash 'd 'a 'e  *hash*)
(add-node-to-hash 'b 'c 'e *hash*)
(add-node-to-hash 'c nil nil *hash*)
(add-node-to-hash 'e 'b 'f *hash*)
(add-node-to-hash 'f 'g nil *hash*)
(add-node-to-hash 'g nil nil *hash*)


;;
;; ------------------------ search code here -------------------------

;;
;; remember where I've been in my search
(defun been-here (node-name)
  (progn
    (format t "~%visited= ~a" node-name)
    (if (not (q-contains node-name))
        (v-add node-name))))


;;
;; add the children  of the current node to the queue for search
;;
;; this adds to the end of the queue - driving breadth 1st search
(defun save-children-b (node-name)
  ; 1st get the left and right child of the current search node
  (let ((left (left (gethash node-name *hash*)))
        (right (right (gethash node-name *hash*))))
    (progn
      (format t "~%(save-p) node= ~a , left= ~a , right= ~a : que= ~a" node-name left right que)
      ; if  the left child is empty, just continue on
      (if (not (null left))
          ; if the left child is already in the queue, just skip it
          (if  (not (q-contains left))
               (progn
                 (q-append left)
                 (format t "~%(save-l) ~a : que= ~a" left que)
                 )))
      
      (if (not (null right))
          (if (not (q-contains right))
              (progn
                (q-append right)
                (format t "~%(save-r) ~a : que= ~a" right que)))
          
          )
      que)
    ))

;;
;; add children to the front of the queue for depth 1st search
(defun save-children (node-name)
  ; 1st get the left and right child of the current search node
  (let ((left (left (gethash node-name *hash*)))
        (right (right (gethash node-name *hash*))))
    (progn
      (format t "~%(save-p) node= ~a , left= ~a , right= ~a : que= ~a" node-name left right que)
      ; if  the left child is empty, just continue on
      (if (not (null left))
          ; if the left child is already in the queue, just skip it
          (if  (not (q-contains left))
               (progn
                 (q-push left)
                 (format t "~%(save-l) ~a : que= ~a" left que)
                 )))

                                     
      (if (not (null right))
                                        ; if the right child is  empty, skip it
          (if (not (q-contains right))
              ; if the right child is already in the queue, continue
              (progn
                (q-push right)
                (format t "~%(save-r) ~a : que= ~a" right que)))
          
          )
      ; show me the queue contents
      que)
    ))

;;
;; the search algorithm
(defun dd ()
  (let ((node (first que)))
    (progn
      ; force a stop if the solution 'g (for goal) is not found
      (if (eq node nil)
          'must-stop
          (progn
            (if (eq node 'g)
                ; if we've found 'g tell us and stop
                (progn
                  (format t "~%found the goal--> ~a: visited= ~a" node visit)
                  'found-it)
                (progn
                  ; did not find it - continue to search
                  (format t "~% --> ~a : ~a" node que)
                  (q-pop)
                  (been-here node)
                  (save-children node)
                  (dd)))
            ))
          )
    ))
  
