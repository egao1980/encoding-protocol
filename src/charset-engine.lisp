(in-package #:encoding-protocol)

;;; Charsets Babel 0.5.0 lacks (issue #7), registered with Babel itself.
;;;
;;; Each charset is a Babel character encoding: DEFINE-CHARACTER-ENCODING plus
;;; DEFINE-ENCODER / -DECODER / -OCTET-COUNTER / -CODE-POINT-COUNTER, the
;;; macros Babel's own enc-*.lisp use (they live in BABEL-ENCODINGS but are not
;;; exported). REGISTER-BABEL-CHARSETS then instantiates the concrete mappings
;;; with Babel's exported INSTANTIATE-CONCRETE-MAPPINGS and adds them to the
;;; tables BABEL:STRING-TO-OCTETS / OCTETS-TO-STRING look up (Babel builds those
;;; once, when it loads). After that every Babel entry point, and so ENCODE /
;;; DECODE here, handles the new names like Babel's own.
;;;
;;; Tables come from tools/gen-charsets.py (Python's codecs): a decode table
;;; plus the encoder's exceptions to its inverse.

(defstruct (code-table (:constructor %make-code-table))
  "Decode table for a single-byte (octets #x80-#xFF) or two-byte charset, and
   the inverse used to encode: code point → packed octets."
  (data (make-array 0 :element-type '(unsigned-byte 16))
   :type (simple-array (unsigned-byte 16) (*)))
  (lead-min #x80 :type (unsigned-byte 8))
  (lead-max #xff :type (unsigned-byte 8))
  (trail-min nil :type (or null (unsigned-byte 8)))
  (trail-max nil :type (or null (unsigned-byte 8)))
  (encode (make-hash-table) :type hash-table))

(defun make-code-table (data &key lead trail overrides)
  "Code table over DATA (0 = unmapped). LEAD and TRAIL are (min max) octet
   ranges for a two-byte table; without them DATA covers octets #x80-#xFF.
   The encoder maps each code point to its first octets in DATA, then
   OVERRIDES ((code-point . packed-octets) or (code-point) for none) apply."
  (let* ((table (%make-code-table :data data
                                  :lead-min (if lead (first lead) #x80)
                                  :lead-max (if lead (second lead) #xff)
                                  :trail-min (first trail)
                                  :trail-max (second trail)))
         (encode (code-table-encode table)))
    (loop for index from 0
          for code across data
          unless (or (zerop code) (gethash code encode))
            do (setf (gethash code encode)
                     (if trail
                         (multiple-value-bind (row column)
                             (floor index (1+ (- (second trail) (first trail))))
                           (logior (ash (+ row (first lead)) 8)
                                   (+ column (first trail))))
                         (+ #x80 index))))
    (loop for (code . packed) in overrides
          do (setf (gethash code encode) (or packed 0)))
    table))

(defmacro define-code-table (name &key documentation lead trail overrides data)
  "Define NAME as a CODE-TABLE. DATA, a literal list, becomes an
   (unsigned-byte 16) vector at macroexpansion time, so the fasl holds it packed."
  `(defparameter ,name
     (make-code-table ,(coerce data '(simple-array (unsigned-byte 16) (*)))
                      :lead ',lead :trail ',trail :overrides ',overrides)
     ,@(when documentation (list documentation))))

(declaim (inline code-table-ref))
(defun code-table-ref (table lead trail)
  "Code point of the two-byte sequence LEAD TRAIL, or NIL."
  (declare (type code-table table) (type (unsigned-byte 8) lead trail))
  (let ((trail-min (code-table-trail-min table))
        (trail-max (code-table-trail-max table)))
    (when (and (<= (code-table-lead-min table) lead (code-table-lead-max table))
               (<= trail-min trail trail-max))
      (let ((code (aref (code-table-data table)
                        (+ (* (- lead (code-table-lead-min table))
                              (1+ (- trail-max trail-min)))
                           (- trail trail-min)))))
        (and (plusp code) code)))))

(declaim (inline code-table-encoding))
(defun code-table-encoding (table code)
  "Packed octets for CODE, or NIL when TABLE cannot encode it."
  (let ((packed (gethash code (code-table-encode table))))
    (and packed (plusp packed) packed)))

(defun %emit-packed (packed emit)
  "Call EMIT on each octet of PACKED, most significant first."
  (declare (function emit))
  (loop for shift downfrom (* 8 (1- (max 1 (ceiling (integer-length packed) 8))))
          to 0 by 8
        do (funcall emit (ldb (byte 8 shift) packed))))

;;; Coding errors, as Babel signals them: honour
;;; *SUPPRESS-CHARACTER-CODING-ERRORS* and return the substitute.

(defun %decode-failure (octets encoding buffer position
                        &optional (type 'babel-encodings:character-decoding-error))
  (unless babel-encodings:*suppress-character-coding-errors*
    (error type :octets octets :encoding encoding :buffer buffer :position position))
  #xfffd)

(defun %encode-failure (code encoding buffer position)
  (unless babel-encodings:*suppress-character-coding-errors*
    (error 'babel-encodings:character-encoding-error
           :code code :encoding encoding :buffer buffer :position position))
  #x1a)

;;; Multibyte charsets: one decode function and one encode function per
;;; charset, shared by Babel's four mapping parts.
;;;
;;;   (decode charset get start end buffer emit)
;;;     GET: index → octet. EMIT: (code-point next-index).
;;;   (encode charset get start end buffer emit char-done)
;;;     GET: index → code point. EMIT: octet. CHAR-DONE: NIL or (next-index).

(defstruct (multibyte-charset (:constructor make-multibyte-charset
                                  (name table &key decode-extra encode-extra)))
  "A Babel encoding NAME over a two-byte CODE-TABLE. DECODE-EXTRA
   (charset get index end buffer) → (values code-point next) or NIL, and
   ENCODE-EXTRA (charset code-point emit) → true when it emitted, cover what the
   table does not (GB18030 four-byte forms, EUC-KR Hangul make-up)."
  (name nil :type keyword)
  (table nil :type code-table)
  (decode-extra nil :type (or null function))
  (encode-extra nil :type (or null function)))

(defun %multibyte-decode-pair (charset get i end buffer)
  (declare (function get) (fixnum i end))
  (let ((table (multibyte-charset-table charset))
        (name (multibyte-charset-name charset))
        (lead (funcall get i)))
    (cond
      ((not (<= (code-table-lead-min table) lead (code-table-lead-max table)))
       (values (%decode-failure (vector lead) name buffer i) (1+ i)))
      ((>= (1+ i) end)
       (values (%decode-failure (vector lead) name buffer i
                                'babel-encodings:end-of-input-in-character)
               end))
      (t
       (let* ((trail (funcall get (1+ i)))
              (code (code-table-ref table lead trail)))
         (if code
             (values code (+ i 2))
             (values (%decode-failure (vector lead trail) name buffer i)
                     ;; An ASCII trail octet starts the next character.
                     (if (< trail #x80) (1+ i) (+ i 2)))))))))

(defun multibyte-decode (charset get start end buffer emit)
  "ASCII below #x80, else DECODE-EXTRA or the two-byte table."
  (declare (function get emit) (fixnum start end))
  (let ((i start)
        (extra (multibyte-charset-decode-extra charset)))
    (declare (fixnum i))
    (loop while (< i end)
          do (let ((octet (funcall get i)))
               (if (< octet #x80)
                   (funcall emit octet (setf i (1+ i)))
                   (multiple-value-bind (code next)
                       (if extra
                           (funcall extra charset get i end buffer)
                           (values nil nil))
                     (unless code
                       (multiple-value-setq (code next)
                         (%multibyte-decode-pair charset get i end buffer)))
                     (funcall emit code (setf i next))))))))

(defun multibyte-encode (charset get start end buffer emit char-done)
  "ASCII below #x80, else the table, else ENCODE-EXTRA."
  (declare (function get emit) (fixnum start end))
  (let ((table (multibyte-charset-table charset))
        (extra (multibyte-charset-encode-extra charset)))
    (loop for i fixnum from start below end
          for code = (funcall get i)
          do (if (< code #x80)
                 (funcall emit code)
                 (let ((packed (code-table-encoding table code)))
                   (cond
                     (packed (%emit-packed packed emit))
                     ((and extra (not (gethash code (code-table-encode table)))
                           (funcall extra charset code emit)))
                     (t (funcall emit (%encode-failure
                                       code (multibyte-charset-name charset)
                                       buffer i))))))
             (when char-done (funcall (the function char-done) (1+ i))))))

;;; Babel mapping templates. Babel calls the factories with its accessor
;;; macros and sequence types when it instantiates concrete mappings.

(defun %decoder-template (decoder charset getter src-type setter dest-type)
  `(lambda (src start end dest d-start)
     (declare (type ,src-type src) (type ,dest-type dest)
              (fixnum start end d-start))
     (let ((di d-start))
       (declare (fixnum di))
       (,decoder ,charset (lambda (i) (,getter src i)) start end src
                 (lambda (code next)
                   (declare (ignore next))
                   (,setter code dest di)
                   (incf di)))
       (the fixnum (- di d-start)))))

(defun %code-point-counter-template (decoder charset getter type)
  `(lambda (seq start end max)
     (declare (type ,type seq) (fixnum start end max))
     (let ((count 0) (stop start))
       (declare (fixnum count stop))
       (block counting
         (,decoder ,charset (lambda (i) (,getter seq i)) start end seq
                   (lambda (code next)
                     (declare (ignore code))
                     (incf count)
                     (setf stop next)
                     (when (and (plusp max) (>= count max))
                       (return-from counting))))
         (setf stop end))
       (values count stop))))

(defun %encoder-template (encoder charset getter src-type setter dest-type)
  `(lambda (src start end dest d-start)
     (declare (type ,src-type src) (type ,dest-type dest)
              (fixnum start end d-start))
     (let ((di d-start))
       (declare (fixnum di))
       (,encoder ,charset (lambda (i) (,getter src i)) start end src
                 (lambda (octet)
                   (,setter octet dest di)
                   (incf di))
                 nil)
       (the fixnum (- di d-start)))))

(defun %octet-counter-template (encoder charset getter type)
  `(lambda (seq start end max)
     (declare (type ,type seq) (fixnum start end max))
     (let ((count 0) (stop start))
       (declare (fixnum count stop))
       (block counting
         (,encoder ,charset (lambda (i) (,getter seq i)) start end seq
                   (lambda (octet)
                     (declare (ignore octet))
                     (incf count))
                   (lambda (next)
                     (setf stop next)
                     (when (and (plusp max) (>= count max))
                       (return-from counting))))
         (setf stop end))
       (values count stop))))

(defmacro define-multibyte-charset (name (&key aliases documentation
                                            (max-units-per-char 4)
                                            (literal-char-code-limit #x80))
                                    &key charset (decoder 'multibyte-decode)
                                      (encoder 'multibyte-encode))
  "Define Babel encoding NAME from CHARSET (a variable) and its DECODER and
   ENCODER functions. Evaluated at compile time too, so REGISTER-BABEL-CHARSETS
   later in the same file sees it."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (babel-encodings::define-character-encoding ,name ,documentation
       :aliases ',aliases
       :max-units-per-char ,max-units-per-char
       :literal-char-code-limit ,literal-char-code-limit)
     (babel-encodings::define-decoder ,name (getter src-type setter dest-type)
       (%decoder-template ',decoder ',charset getter src-type setter dest-type))
     (babel-encodings::define-code-point-counter ,name (getter type)
       (%code-point-counter-template ',decoder ',charset getter type))
     (babel-encodings::define-encoder ,name (getter src-type setter dest-type)
       (%encoder-template ',encoder ',charset getter src-type setter dest-type))
     (babel-encodings::define-octet-counter ,name (getter type)
       (%octet-counter-template ',encoder ',charset getter type))))

(defmacro define-single-byte-charset (name table (&key aliases documentation))
  "Define Babel encoding NAME: ASCII below #x80, TABLE (a CODE-TABLE variable)
   above, with Babel's unibyte coder macros."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (babel-encodings::define-character-encoding ,name ,documentation
       :aliases ',aliases
       :literal-char-code-limit #x80)
     (babel-encodings::define-unibyte-decoder ,name (octet)
       (if (< octet #x80)
           octet
           (let ((code (aref (code-table-data ,table) (- octet #x80))))
             (if (zerop code)
                 (babel-encodings::handle-error)
                 code))))
     (babel-encodings::define-unibyte-encoder ,name (code)
       (if (< code #x80)
           code
           (or (code-table-encoding ,table code)
               (babel-encodings::handle-error))))))

;;; Accessors for INSTANTIATE-CONCRETE-MAPPINGS (Babel's own are internal).

(defmacro %ub-get (vector index)
  `(aref ,vector ,index))

(defmacro %ub-set (value vector index)
  `(setf (aref ,vector ,index) ,value))

(defmacro %string-get (string index)
  `(char-code (schar ,string ,index)))

(defmacro %string-set (code string index)
  `(setf (schar ,string ,index) (code-char ,code)))

(defun %add-babel-mappings (into from)
  (maphash (lambda (name mapping) (setf (gethash name into) mapping)) from)
  into)

(defmacro register-babel-charsets (&rest names)
  "Instantiate Babel's concrete mappings for the encodings NAMES and add them to
   the tables Babel's string functions use (BABEL::*STRING-VECTOR-MAPPINGS*, and
   on SBCL the simple-base-string encoders)."
  `(progn
     (%add-babel-mappings
      babel::*string-vector-mappings*
      (babel-encodings:instantiate-concrete-mappings
       :encodings ,names
       :octet-seq-getter %ub-get
       :octet-seq-setter %ub-set
       :octet-seq-type (simple-array (unsigned-byte 8) (*))
       :code-point-seq-getter %string-get
       :code-point-seq-setter %string-set
       :code-point-seq-type babel:simple-unicode-string))
     #+sbcl
     (%add-babel-mappings
      babel::*simple-base-string-vector-mappings*
      (babel-encodings:instantiate-concrete-mappings
       :encodings ,names
       :instantiate-decoders nil
       :octet-seq-getter %ub-get
       :octet-seq-setter %ub-set
       :octet-seq-type (simple-array (unsigned-byte 8) (*))
       :code-point-seq-getter %string-get
       :code-point-seq-setter %string-set
       :code-point-seq-type simple-base-string))
     ',names))
