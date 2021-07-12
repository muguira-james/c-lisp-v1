(defpackage simulation/tests/main
  (:use :cl
        :simulation
        :rove))
(in-package :simulation/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :simulation)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
