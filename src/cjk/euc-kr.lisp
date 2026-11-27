(in-package #:encoding-protocol)

;;; EUC-KR and CP949 (issue #7): system encoding-protocol/euc-kr, loaded on
;;; first use (issue #12). Tables: euc-kr-table.lisp and cp949-table.lisp,
;;; from tools/gen-charsets.py. The references are Python's euc_kr and cp949
;;; codecs.
;;;
;;; CP949 (Unified Hangul Code) is a two-byte table. EUC-KR is KS X 1001,
;;; plus KS X 1001:1998 Annex 3 make-up sequences for the Hangul syllables
;;; KS X 1001 lacks: A4 D4, then A4 + initial, A4 + medial, A4 + final jamo
;;; (Python euc_kr encodes and decodes them).

(defparameter *cp949* (make-multibyte-charset :cp949 *cp949-table*))

(define-multibyte-charset :cp949
    (:aliases (:uhc :windows-949) :max-units-per-char 2
     :documentation "CP949 / Unified Hangul Code: Korean, a superset of EUC-KR
   (Python cp949). Mail labelled ks_c_5601-1987 is CP949.")
  :charset *cp949*)

(defun %euc-kr-decode-make-up (charset get i end buffer)
  (declare (function get) (fixnum i end))
  (when (and (= (funcall get i) #xa4)
             (< (1+ i) end)
             (= (funcall get (1+ i)) #xd4))
    (flet ((fail (type)
             (return-from %euc-kr-decode-make-up
               (values (%decode-failure (vector #xa4 #xd4) (multibyte-charset-name charset)
                                        buffer i type)
                       (min end (+ i 2))))))
      (when (> (+ i 8) end)
        (fail 'babel-encodings:end-of-input-in-character))
      (destructuring-bind (initials medials finals) *euc-kr-jamo*
        (let ((l (and (= (funcall get (+ i 2)) #xa4) (position (funcall get (+ i 3)) initials)))
              (v (and (= (funcall get (+ i 4)) #xa4) (position (funcall get (+ i 5)) medials)))
              (tt (and (= (funcall get (+ i 6)) #xa4) (position (funcall get (+ i 7)) finals))))
          (unless (and l v tt)
            (fail 'babel-encodings:character-decoding-error))
          (values (+ #xac00 (* (+ (* l 21) v) 28) tt) (+ i 8)))))))

(defun %euc-kr-encode-make-up (charset code emit)
  (declare (ignore charset) (function emit))
  (when (<= #xac00 code #xd7a3)
    (destructuring-bind (initials medials finals) *euc-kr-jamo*
      (multiple-value-bind (lv tt) (floor (- code #xac00) 28)
        (multiple-value-bind (l v) (floor lv 21)
          (dolist (octet (list #xa4 #xd4 #xa4 (aref initials l) #xa4 (aref medials v)
                               #xa4 (aref finals tt)))
            (funcall emit octet))
          t)))))

(defparameter *euc-kr*
  (make-multibyte-charset :euc-kr *euc-kr-table*
                          :decode-extra #'%euc-kr-decode-make-up
                          :encode-extra #'%euc-kr-encode-make-up))

(define-multibyte-charset :euc-kr
    (:aliases (:euckr) :max-units-per-char 8
     :documentation "EUC-KR: KS X 1001 with Annex 3 Hangul make-up sequences
   (Python euc_kr).")
  :charset *euc-kr*)

(register-babel-charsets :cp949 :euc-kr)
