(in-package #:encoding-protocol)

;;; RFC 4648 Base16 / Base32 / Base32hex / Base64 / Base64url.

(defparameter +base64-alphabet+
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

(defparameter +base64url-alphabet+
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_")

(defparameter +base32-alphabet+
  "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567")

(defparameter +base32hex-alphabet+
  "0123456789ABCDEFGHIJKLMNOPQRSTUV")

(defparameter +base16-alphabet+
  "0123456789ABCDEF")

(defstruct rfc4648-spec
  name
  alphabet
  table
  bits
  group
  fold-case)

(defun %make-decode-table (alphabet &key fold-case)
  (let ((tbl (make-array 256 :initial-element nil)))
    (loop for i from 0 below (length alphabet)
          for c = (char alphabet i)
          do (setf (aref tbl (char-code c)) i)
             (when fold-case
               (let ((down (char-downcase c))
                     (up (char-upcase c)))
                 (when (char/= down c)
                   (setf (aref tbl (char-code down)) i))
                 (when (char/= up c)
                   (setf (aref tbl (char-code up)) i)))))
    tbl))

(defun %spec (name alphabet bits group &key fold-case)
  (make-rfc4648-spec :name name
                     :alphabet alphabet
                     :table (%make-decode-table alphabet :fold-case fold-case)
                     :bits bits
                     :group group
                     :fold-case fold-case))

(defparameter *rfc4648-specs*
  (list (%spec :base64 +base64-alphabet+ 6 4)
        (%spec :base64url +base64url-alphabet+ 6 4)
        (%spec :base32 +base32-alphabet+ 5 8 :fold-case t)
        (%spec :base32hex +base32hex-alphabet+ 5 8 :fold-case t)
        (%spec :base16 +base16-alphabet+ 4 2 :fold-case t)))

(defun find-rfc4648-spec (encoding)
  (find encoding *rfc4648-specs* :key #'rfc4648-spec-name :test #'eq))

(defun %wrap-columns (string columns &key (break (format nil "~C~C" #\Return #\Newline)))
  "Insert BREAK every COLUMNS characters. No trailing break."
  (cond
    ((or (null columns) (not (plusp columns)) (< (length string) columns))
     string)
    (t
     (let ((out (make-string-output-stream))
           (n (length string)))
       (loop for i from 0 below n by columns
             for end = (min n (+ i columns))
             do (write-string string out :start i :end end)
                (when (< end n)
                  (write-string break out)))
       (get-output-stream-string out)))))

(defun %whitespace-p (c)
  (or (char= c #\Space) (char= c #\Tab)
      (char= c #\Return) (char= c #\Newline)))

(defun %as-octets (value)
  (etypecase value
    ((vector (unsigned-byte 8)) value)
    (string (babel:string-to-octets value :encoding :utf-8))
    (vector
     (if (and (not (stringp value))
              (every (lambda (b) (typep b '(unsigned-byte 8))) value))
         (coerce value '(vector (unsigned-byte 8)))
         (error 'encoding-encode-error :message "cannot treat value as octets")))))

(defun %as-ascii-string (source)
  (etypecase source
    (string source)
    ((vector (unsigned-byte 8))
     (babel:octets-to-string source :encoding :ascii))
    (vector
     (babel:octets-to-string (coerce source '(vector (unsigned-byte 8)))
                             :encoding :ascii))
    (stream
     (let ((out (make-string-output-stream)))
       (loop for c = (read-char source nil nil)
             while c
             do (write-char c out))
       (get-output-stream-string out)))))

(defun %encode-octets (octets spec &key (pad t))
  (let* ((n (length octets))
         (bits (rfc4648-spec-bits spec))
         (alphabet (rfc4648-spec-alphabet spec))
         (group (rfc4648-spec-group spec))
         (raw (if (zerop n) 0 (ceiling (* n 8) bits)))
         (out-len (if pad (* group (ceiling raw group)) raw))
         (out (make-string out-len))
         (acc 0)
         (have 0)
         (j 0))
    (loop for i from 0 below n
          do (setf acc (logior (ash acc 8) (aref octets i))
                   have (+ have 8))
             (loop while (>= have bits)
                   do (decf have bits)
                      (setf (char out j)
                            (char alphabet (ldb (byte bits have) acc)))
                      (incf j)
                      (setf acc (logand acc (1- (ash 1 have))))))
    (when (plusp have)
      (setf (char out j) (char alphabet (ash acc (- bits have))))
      (incf j))
    (when pad
      (loop while (< j out-len)
            do (setf (char out j) #\=)
               (incf j)))
    out))

(defun %digit (table c)
  (let ((code (char-code c)))
    (when (< code 256)
      (aref table code))))

(defun %invalid-char (c spec)
  (restart-case
      (error 'encoding-decode-error
             :character c
             :message (format nil "invalid ~A character ~S"
                              (rfc4648-spec-name spec) c))
    (continue ()
      :report "Skip this character"
      nil)
    (use-value (value)
      :report "Use a supplied alphabet index"
      :interactive (lambda ()
                     (format *query-io* "Alphabet index: ")
                     (force-output *query-io*)
                     (list (read *query-io*)))
      value)))

(defun %decode-string (string spec &key (pad t) strict)
  (let* ((bits (rfc4648-spec-bits spec))
         (table (rfc4648-spec-table spec))
         (group (rfc4648-spec-group spec))
         (n (length string))
         (out (make-array (ceiling (* n bits) 8)
                          :element-type '(unsigned-byte 8)
                          :fill-pointer 0))
         (acc 0)
         (have 0)
         (saw-pad nil)
         (digit-count 0)
         (pad-count 0))
    (loop for i from 0 below n
          for c = (char string i)
          do (cond
               ((%whitespace-p c)
                (when strict
                  (error 'encoding-decode-error
                         :character c
                         :message "whitespace is forbidden in strict mode")))
               ((char= c #\=)
                (incf pad-count)
                (setf saw-pad t))
               (t
                (when saw-pad
                  (error 'encoding-decode-error
                         :character c
                         :message "data after pad character"))
                (let ((v (or (%digit table c) (%invalid-char c spec))))
                  (when v
                    (unless (and (integerp v) (<= 0 v (1- (ash 1 bits))))
                      (error 'encoding-decode-error
                             :message (format nil "alphabet index ~S out of range" v)))
                    (setf acc (logior (ash acc bits) v)
                          have (+ have bits))
                    (incf digit-count)
                    (when (>= have 8)
                      (decf have 8)
                      (vector-push (ldb (byte 8 have) acc) out)
                      (setf acc (logand acc (1- (ash 1 have))))))))))
    (when strict
      (when (and (plusp have) (plusp (logand acc (1- (ash 1 have)))))
        (error 'encoding-decode-error :message "non-zero leftover bits"))
      (cond
        (pad
         (let ((total (+ digit-count pad-count)))
           (unless (zerop (mod total group))
             (error 'encoding-decode-error
                    :message (format nil "length ~D is not a multiple of ~D"
                                     total group)))))
        ((plusp pad-count)
         (error 'encoding-decode-error :message "unexpected pad character"))))
    (coerce out '(vector (unsigned-byte 8)))))
