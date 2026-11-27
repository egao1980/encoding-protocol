(in-package #:encoding-protocol)

;;; ISO-2022-JP (issue #7): system encoding-protocol/iso-2022-jp, loaded on
;;; first use (issue #12). Tables: jisx0208-table.lisp, from
;;; tools/gen-charsets.py. The reference is Python's iso2022_jp codec.

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

(register-babel-charsets :iso-2022-jp)
