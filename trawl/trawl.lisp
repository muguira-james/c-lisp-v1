
;;
;; start of a news trawler
;;
;; 1st get the rss feed reader working - done
;; 2nd get the feed config file working - partly done
;; 3rd put feed config file in file and read from it
;;
;; ideas
;; - web service: for a given input url return a list of news items
;; - cmd line: return a list of news items
;;
(defpackage :trawl
  (:use :cl)
  (:export
   ;; returns a list from cnn us only
   #:get-list-of-us-titles
   ;; return list from input url
   #:list-of-titles))

(ql:quickload :rss)

(in-package :trawl)

(defstruct aFeed
  (source "")
  (link ""))


;;
;; make a list of feeds
(defvar feeds "")

(setf feeds (list
               (make-aFeed
                :source "cnn-us"
                :link "http://rss.cnn.com/rss/cnn_us.rss")
               (make-aFeed
                :source "bbc-news"
                :link "http://feeds.bbci.co.uk/news/rss.xml")))

;;
;; one way to get the link from a feed structure
(defun feed-link (item)
  (aFeed-link item))

;;
;; use a lmbda to loop through the feeds list
(mapcar #'(lambda (item) (aFeed-link item)) feeds)


;; example of how to use cl-rss:rss-site
;; (defvar url "http://rss.cnn.com/rss/cnn_us.rss")
;; (defvar cnn (rss:rss-site url))


;; spit out the title of the first item
;; (rss:title (first (rss:items cnn)))

;;
;; return the title from input item
(defun get-title (item)
  (rss:title item))

;;
;; use hard coded url to return a list of rss news items
(defun get-list-of-us-titles ()
  (let ((site (rss:rss-site "http://rss.cnn.com/rss/cnn_us.rss")))
    (mapcar #'get-title (rss:items site))))

;;
;; use input url to return a list of rss news items
(defun list-of-titles (url)
  (let ((site (rss:rss-site url)))
    (mapcar #'get-title (rss:items site))))

;; example of how to use
;; (trawl:list-of-titles "http://rss.cnn.com/rss/cnn_us.rss")

;; another example from the command line
;; sbcl --load ./trawl.lisp
;; ... * (trawl:list-of-titles "http://rss.cnn.com/rss/cnn_us.rss")
