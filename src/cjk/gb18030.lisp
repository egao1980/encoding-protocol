(in-package #:encoding-protocol)

;;; GB18030 (issue #7): system encoding-protocol/gb18030, loaded on first use
;;; (issue #12). Tables: gb18030-table.lisp, from tools/gen-charsets.py. The
;;; reference is Python's gb18030 codec.
;;;
;;; The two-byte table (GBK and its extensions), four-byte forms b1 b2 b3 b4
;;; (81-FE 30-39 81-FE 30-39) with linear index
;;; ((b1-#x81)*10 + b2-#x30)*1260 + (b3-#x81)*10 + b4-#x30: BMP code points by
;;; *GB18030-RANGES* below 39420, supplementary planes from 189000.

(defconstant +gb18030-bmp-limit+ 39420)

(defconstant +gb18030-supplementary-base+ 189000)

(defun %gb18030-linear-code (linear)
  "Code point of four-byte LINEAR index, or NIL."
  (let ((ranges *gb18030-ranges*))
    (cond
      ((< linear +gb18030-bmp-limit+)
       (let ((low 0) (high (1- (floor (length ranges) 3))))
         (loop while (<= low high)
               do (let* ((mid (floor (+ low high) 2))
                         (start (aref ranges (* 3 mid))))
                    (cond ((< linear start) (setf high (1- mid)))
                          ((>= linear (+ start (aref ranges (+ 2 (* 3 mid)))))
                           (setf low (1+ mid)))
                          (t (return (+ (aref ranges (1+ (* 3 mid))) (- linear start)))))))))
      ((< -1 (- linear +gb18030-supplementary-base+) #x100000)
       (+ #x10000 (- linear +gb18030-supplementary-base+))))))

(defun %gb18030-code-linear (code)
  "Four-byte linear index of CODE, or NIL."
  (if (>= code #x10000)
      (+ +gb18030-supplementary-base+ (- code #x10000))
      (let* ((ranges *gb18030-ranges*)
             (low 0) (high (1- (floor (length ranges) 3))))
        (loop while (<= low high)
              do (let* ((mid (floor (+ low high) 2))
                        (first (aref ranges (1+ (* 3 mid)))))
                   (cond ((< code first) (setf high (1- mid)))
                         ((>= code (+ first (aref ranges (+ 2 (* 3 mid)))))
                          (setf low (1+ mid)))
                         (t (return (+ (aref ranges (* 3 mid)) (- code first))))))))))

(defun %gb18030-decode-four (charset get i end buffer)
  (declare (function get) (fixnum i end))
  (when (and (< (1+ i) end) (<= #x30 (funcall get (1+ i)) #x39))
    (let ((b1 (funcall get i)))
      (flet ((fail (type)
               (return-from %gb18030-decode-four
                 (values (%decode-failure (vector b1) (multibyte-charset-name charset)
                                          buffer i type)
                         (1+ i)))))
        (unless (<= #x81 b1 #xfe)
          (fail 'babel-encodings:character-decoding-error))
        (when (>= (+ i 3) end)
          (fail 'babel-encodings:end-of-input-in-character))
        (let ((b2 (funcall get (1+ i)))
              (b3 (funcall get (+ i 2)))
              (b4 (funcall get (+ i 3))))
          (unless (and (<= #x81 b3 #xfe) (<= #x30 b4 #x39))
            (fail 'babel-encodings:character-decoding-error))
          (let ((code (%gb18030-linear-code
                       (+ (* (+ (* (+ (* (- b1 #x81) 10) (- b2 #x30)) 126) (- b3 #x81)) 10)
                          (- b4 #x30)))))
            (unless code
              (fail 'babel-encodings:character-decoding-error))
            (values code (+ i 4))))))))

(defun %gb18030-encode-four (charset code emit)
  (declare (ignore charset) (function emit))
  (let ((linear (%gb18030-code-linear code)))
    (when linear
      (multiple-value-bind (rest b4) (floor linear 10)
        (multiple-value-bind (rest b3) (floor rest 126)
          (multiple-value-bind (b1 b2) (floor rest 10)
            (funcall emit (+ b1 #x81))
            (funcall emit (+ b2 #x30))
            (funcall emit (+ b3 #x81))
            (funcall emit (+ b4 #x30))
            t))))))

(defparameter *gb18030*
  (make-multibyte-charset :gb18030 *gb18030-table*
                          :decode-extra #'%gb18030-decode-four
                          :encode-extra #'%gb18030-encode-four))

(define-multibyte-charset :gb18030
    (:aliases () :max-units-per-char 4
     :documentation "GB18030: every Unicode code point (Python gb18030).")
  :charset *gb18030*)

(register-babel-charsets :gb18030)
