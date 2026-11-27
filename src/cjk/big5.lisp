(in-package #:encoding-protocol)

;;; Big5 (issue #7): system encoding-protocol/big5, loaded on first use
;;; (issue #12). Table: big5-table.lisp, from tools/gen-charsets.py. The
;;; reference is Python's big5 codec.

(defparameter *big5* (make-multibyte-charset :big5 *big5-table*))

(define-multibyte-charset :big5
    (:aliases (:big5-tw :csbig5) :max-units-per-char 2
     :documentation "Big5: Traditional Chinese (Python big5).")
  :charset *big5*)

(register-babel-charsets :big5)
