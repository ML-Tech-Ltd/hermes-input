(defpackage overmind-input/tests/main
  (:use :cl
        :overmind-input
        :rove))
(in-package :overmind-input/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :overmind-input)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
