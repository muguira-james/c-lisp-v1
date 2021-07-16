(defpackage clos-shape
  (:use :cl))
(in-package :clos-shape)

(defstruct triangle
  (base 0)
  (height 0))

(defstruct rectangle
  (width 0)
  (length 0))

(defstruct circle
  (radius 0))



(defmethod area ((figure triangle))
  (* 1/2
     (triangle-base figure)
     (triangle-height figure)))
