
;; define a global to hold the db while we operate on it
(defvar *db* nil)

;; helper to return a plist from inputs
(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

;;
;; build record objects
(defun add-record (cd) (push cd *db*))

;;
;; sample data
;; (add-record (make-cd "Devil went down to Georgia" "Charlie Daniels Band" 8 t))

;;
;; pretty print the db
(defun dump-db ()
  (dolist (cd *db*)
    (format t "~{~a:~10t~a~%~}~%" cd)))

;;
;; save the db (as s-expressions) to a file
(defun save-db (filename)
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

;;
;; load the db from a file (note: filename is absolute path)
(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))

;;
;; select stuff in db
;;
;; how to use:
;; (select (where :rating 8)) -> gives 3
;; (select (where :ripped t)) -> gives just "devil went down to georgia"
(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

(defun where (&key title artist rating (ripped nil ripped-p))
  #'(lambda (cd)
      (and
       (if title    (equal (getf cd :title)  title)  t)
       (if artist   (equal (getf cd :artist) artist) t)
       (if rating   (equal (getf cd :rating) rating) t)
       (if ripped-p (equal (getf cd :ripped) ripped) t))))


(defun update (selector-fn &key title artist rating (ripped nil ripped-p))
  (setf *db*
        (mapcar
         #'(lambda (row)
             (when (funcall selector-fn row)
               (if title    (setf (getf row :title)  title))
               (if artist   (setf (getf row :artist) artist))
               (if rating   (setf (getf row :rating) rating))
               (if ripped-p (setf (getf row :ripped) ripped)))
             row) *db*)))


           
