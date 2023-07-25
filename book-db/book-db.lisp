;; build an example book db
;;
;; define each entry to be an association list of title, author, classification
;;
;; this make assumptions!!
;; 1. a book is (in this order): title author classification
;;    may be fix with a book being a hash?
(defun make-book (title author classification)
  (list (list 'title title)
        (list 'author author)
        (list 'classification classification)))

(defun book-author (book)
  (second (assoc 'author book)))

(defun book-title (book)
  (second (assoc 'title book)))

(defun book-classification (book)
  (second (assoc 'classification book)))

;;
;; broken - just adds another author - I want to replace
;; (defun book-author-writer (book author)
;;  (cons (list 'author author) book))

(defun book-author-writer (book author)
  (if (eql 'author (first (first book)))
           (cons (list 'author author) (rest book))
           (cons (first book)
                 (book-author-writer (rest book) author))))
                          

;; test a single book
(defvar abook (make-book '(Levels of conceptual Interoperability)
                         '(muguira)
                         '(research)))

;; test
(book-author-writer abook '(james muguira))

(defvar afictionbook (make-book '(tom swayer)
                            '(mark twain)
                            '(fiction)))

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

;; some tests
(book-author (second *book-db*))
;; -> (muguira)
(book-title (first *book-db*))
;; -> (artificial intelligence)
(book-classification (first *book-db*))
;; -> (technical AI)
(book-classification (second *book-db*))
;; -> (research)

;;
;; create some operators: filter, transform, ...
(defun list-authors (books)
  (if (endp books)
      nil
      (cons (book-author (first books))
            (list-authors (rest books)))))

(defun list-titles (books)
  (if (endp books)
      nil
      (cons (book-title (first books))
            (list-titles (rest books)))))

(defun list-fiction-books (books)
  (cond ((endp books) nil)
        ((fictionp (first books))
         (cons (first books)
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
