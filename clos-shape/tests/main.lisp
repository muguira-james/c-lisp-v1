(defpackage clos-shape/tests/main
  (:use :cl
        :clos-shape
        :rove))
(in-package :clos-shape/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :clos-shape)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
