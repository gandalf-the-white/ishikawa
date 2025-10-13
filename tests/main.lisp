(defpackage ishikawa/tests/main
  (:use :cl
        :ishikawa
        :rove))
(in-package :ishikawa/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :ishikawa)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
