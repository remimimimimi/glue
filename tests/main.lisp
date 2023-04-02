(defpackage glue/tests/main
  (:use :cl
        :glue
        :rove))
(in-package :glue/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :glue)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
