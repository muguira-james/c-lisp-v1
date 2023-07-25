;; build an example book db
;;
;; HASH-table version
;;
;; define book-db to be a list of nodes.
;;   Each node is a hash-table the following keys:
;;   title, author, classification
;;
(defun make-book (title author classification)
  (let ((hh (make-hash-table)))
    (setf (gethash 'title hh) title)
    (setf (gethash 'author hh) author)
    (setf (gethash 'classification hh) classification)
    hh))


;; examples of readers (the next 3)
;;
;; show the title for htis book item
(defun book-author (book)
  (gethash 'author book))


;;
;; show the author for this item
(defun book-title (book)
  (gethash 'title book))

;;
;; show the classification for this item
(defun book-classification (book)
  (gethash 'classification book))

;;
;; this is a n example of a writer
;;
;; change the author for this book item
(defun book-author-writer (book author)
  (setf (gethash 'author book) author))



;; test a single book
(defvar abook (make-book '(Levels of conceptual Interoperability)
                         '(muguira)
                         '(research)))

;; test
(book-author-writer abook '(james muguira))
;; will change author to '(james muguira)


(defvar afictionbook (make-book '(tom swayer)
                            '(mark twain)
                            '(fiction)))

;; some tests
(book-author afictionbook)
;; -> (mark twain)
(book-title abook)
;; -> (top swayer)
(book-classification abook)
;; -> (fiction)


(defun fictionp (book)
  (member 'fiction (book-classification book)))
;;
;; define the book-db
(defvar *book-db*)
;;
;; define the list
(setf *book-db* (list
                 (make-book '(atificial intelligence)
                            '(patric winston)
                            '(technical ai))
                 (make-book '(Levels of conceptual interoperability)
                            '(muguira)
                            '(research))
                 (make-book '(tom swayer)
                            '(mark twain)
                            '(fiction))
                 (make-book '(bourne identity)
                            '(robert ludlum)
                            '(fiction))))

(book-classification (second *book-db*))
;; -> (research)

;;
;; create some operators: filter, transform, ...
(defun list-authors (books)
  (if (endp books)
      nil
      (progn
       (format t "author: ~S~%" (gethash 'author (first books)))
       (list-authors (rest books)))))

(defun list-titles (books)
  (if (endp books)
      nil
      ;;(format t "title: ~S~%" (gethash 'title (first books)))
      (cons (gethash 'title (first books))
            (list-titles (rest books)))))


(defun list-fiction-books (books)
  (cond ((endp books) nil)
        ((fictionp (first books))
         (cons (gethash 'title (first books))
               (list-fiction-books (rest books))))
        (t (list-fiction-books (rest books)))))

;; test
(length (list-titles *book-db*))
;; -> 4

;; now change tack ... use mapcar
;;
;; list titles
(mapcar #'book-author *book-db*)
;;
;; list authors
(mapcar #'book-title *book-db*)
;;
;; list classifications
(mapcar #'book-classification *book-db*)


;;
;; pretty print a specific book item (hash-table)
(defun print-hash-item (hash)
  (let ((title (gethash 'title hash))
        (author (gethash 'author hash))
        (classification (gethash 'classification hash)))
    (format t "title: ~S~%author: ~S~%classification: ~S~%~%" title author classification)))

;;
;; pretty print the book db
(defun list-books (books)
  (if (endp books)
      nil
      (progn
       (print-hash-item (first books))
       (list-books (rest books)))))
                        

;; test
(list-books *book-db*)
  
