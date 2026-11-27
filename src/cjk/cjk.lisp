(in-package #:encoding-protocol)

;;; Multibyte charsets Babel lacks (issue #7): system encoding-protocol/cjk.
;;; Tables: *-table.lisp beside this file, from tools/gen-charsets.py. The
;;; reference for every charset is Python's codec of the same name.

;;; Big5, CP949 (Unified Hangul Code): two-byte tables.

(defparameter *big5* (make-multibyte-charset :big5 *big5-table*))

(define-multibyte-charset :big5
    (:aliases (:big5-tw :csbig5) :max-units-per-char 2
     :documentation "Big5: Traditional Chinese (Python big5).")
  :charset *big5*)

(defparameter *cp949* (make-multibyte-charset :cp949 *cp949-table*))

(define-multibyte-charset :cp949
    (:aliases (:uhc :windows-949) :max-units-per-char 2
     :documentation "CP949 / Unified Hangul Code: Korean, a superset of EUC-KR
   (Python cp949). Mail labelled ks_c_5601-1987 is CP949.")
  :charset *cp949*)

;;; EUC-KR: KS X 1001, plus KS X 1001:1998 Annex 3 make-up sequences for the
;;; Hangul syllables KS X 1001 lacks: A4 D4, then A4 + initial, A4 + medial,
;;; A4 + final jamo (Python euc_kr encodes and decodes them).

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

;;; GB18030: the two-byte table (GBK and its extensions), four-byte forms
;;; b1 b2 b3 b4 (81-FE 30-39 81-FE 30-39) with linear index
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

;;; ISO-2022-JP (RFC 1468): stateful. G0 is ASCII (ESC ( B), JIS X 0201
;;; Roman (ESC ( J) or JIS X 0208 (ESC $ B, ESC $ @). Decoding follows
;;; CPython's iso2022 decoder: control octets pass through in any mode, G1
;;; designations are accepted and ignored, ESC & @ ESC $ B announces JIS X
;;; 0208-1990, and an ESC not followed by ( ) $ . & passes through with the
;;; octets after it up to a final byte. Encoding switches mode per character
;;; and ends in ASCII.

(defparameter *iso-2022-jp* (make-multibyte-charset :iso-2022-jp *jisx0208-table*))

(declaim (inline %escape-final-p))
(defun %escape-final-p (octet)
  (or (<= #x41 octet #x5a) (= octet #x40)))

(defun %iso-2022-jp-escape (get i end)
  "Parse the escape sequence at I. (values action length): ACTION is :ASCII,
   :ROMAN or :JIS (designate G0), :IGNORE (G1), :THROUGH (not ISO 2022),
   :INCOMPLETE or :ILLEGAL."
  (declare (function get) (fixnum i end))
  (when (>= (1+ i) end)
    (return-from %iso-2022-jp-escape (values :incomplete 1)))
  (unless (member (funcall get (1+ i)) '(#x28 #x29 #x24 #x2e #x26))
    (return-from %iso-2022-jp-escape (values :through 1)))
  (let ((length 0) (k 1))
    (declare (fixnum length k))
    (loop while (< k 16)
          do (when (>= (+ i k) end)
               (return-from %iso-2022-jp-escape (values :incomplete 1)))
             (let ((octet (funcall get (+ i k))))
               (when (%escape-final-p octet)
                 (setf length (1+ k))
                 (return))
               (when (and (= octet #x26) (< (+ i k 1) end)
                          (= (funcall get (+ i k 1)) #x40))
                 (incf k 2)))
             (incf k))
    (flet ((at (n) (funcall get (+ i n))))
      (multiple-value-bind (dbcs final g1)
          (case length
            (3 (case (at 1)
                 (#x24 (values t (at 2) nil))
                 (#x28 (values nil (at 2) nil))
                 (#x29 (values nil (at 2) t))))
            (4 (when (and (= (at 1) #x24) (member (at 2) '(#x28 #x29)))
                 (values t (at 3) (= (at 2) #x29))))
            (6 (when (and (= (at 3) #x1b) (= (at 4) #x24) (= (at 5) #x42))
                 (values t #x42 nil))))
        (let ((mode (and final
                         (if dbcs
                             (and (member final '(#x42 #x40)) :jis)
                             (case final (#x42 :ascii) (#x4a :roman))))))
          (cond ((null mode) (values :illegal (max length 1)))
                (g1 (values :ignore length))
                (t (values mode length))))))))

(defun %iso-2022-jp-decode (charset get start end buffer emit)
  (declare (function get emit) (fixnum start end))
  (let ((i start)
        (g0 :ascii)
        (through nil)
        (table (multibyte-charset-table charset))
        (name (multibyte-charset-name charset)))
    (declare (fixnum i))
    (loop while (< i end)
          do (let ((c (funcall get i)))
               (cond
                 (through
                  (setf through (not (%escape-final-p c)))
                  (funcall emit c (incf i)))
                 ((= c #x1b)
                  (multiple-value-bind (action length) (%iso-2022-jp-escape get i end)
                    (case action
                      (:through
                       (setf through t)
                       (funcall emit c (incf i)))
                      ((:ascii :roman :jis)
                       (setf g0 action)
                       (incf i length))
                      (:ignore (incf i length))
                      (t
                       (funcall emit
                                (%decode-failure (vector c) name buffer i
                                                 (if (eq action :incomplete)
                                                     'babel-encodings:end-of-input-in-character
                                                     'babel-encodings:character-decoding-error))
                                (incf i length))))))
                 ((>= c #x80)
                  (funcall emit (%decode-failure (vector c) name buffer i) (incf i)))
                 ((or (< c #x20) (eq g0 :ascii))
                  (funcall emit c (incf i)))
                 ((eq g0 :roman)
                  (funcall emit (or (car (rassoc c *jisx0201-roman*)) c) (incf i)))
                 ((>= (1+ i) end)
                  (funcall emit (%decode-failure (vector c) name buffer i
                                                 'babel-encodings:end-of-input-in-character)
                           (setf i end)))
                 (t
                  (let* ((trail (funcall get (1+ i)))
                         (code (code-table-ref table c trail)))
                    (if code
                        (funcall emit code (incf i 2))
                        (funcall emit (%decode-failure (vector c trail) name buffer i)
                                 (incf i))))))))))

(defun %iso-2022-jp-encode (charset get start end buffer emit char-done)
  (declare (function get emit) (fixnum start end))
  (let ((mode :ascii)
        (table (multibyte-charset-table charset)))
    (flet ((switch (want)
             (unless (eq want mode)
               (dolist (octet (ecase want
                                (:ascii '(#x1b #x28 #x42))
                                (:roman '(#x1b #x28 #x4a))
                                (:jis '(#x1b #x24 #x42))))
                 (funcall emit octet))
               (setf mode want))))
      (loop for i fixnum from start below end
            for code = (funcall get i)
            do (let (packed roman)
                 (cond
                   ((< code #x80)
                    (switch :ascii)
                    (funcall emit code))
                   ((setf packed (code-table-encoding table code))
                    (switch :jis)
                    (%emit-packed packed emit))
                   ((setf roman (cdr (assoc code *jisx0201-roman*)))
                    (switch :roman)
                    (funcall emit roman))
                   (t
                    (switch :ascii)
                    (funcall emit (%encode-failure code (multibyte-charset-name charset)
                                                   buffer i)))))
               (when char-done (funcall (the function char-done) (1+ i))))
      (switch :ascii))))

(define-multibyte-charset :iso-2022-jp
    (:aliases (:csiso2022jp) :max-units-per-char 8 :literal-char-code-limit 0
     :documentation "ISO-2022-JP (RFC 1468): ASCII, JIS X 0201 Roman and JIS X
   0208 with escape sequences (Python iso2022_jp).")
  :charset *iso-2022-jp*
  :decoder %iso-2022-jp-decode
  :encoder %iso-2022-jp-encode)

(register-babel-charsets :big5 :cp949 :euc-kr :gb18030 :iso-2022-jp)
