(defpackage hermes-input/tests/main
  (:use :cl
        :hermes-input
        :rove))
(in-package :hermes-input/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :hermes-input)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
           (ok (= 1 1))))
