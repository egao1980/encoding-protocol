(defpackage #:encoding-protocol/tests
  (:use #:cl #:rove #:encoding-protocol))

(in-package #:encoding-protocol/tests)

(defun utf8 (string)
  (encode string))
