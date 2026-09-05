(in-package #:encoding-protocol)

(defvar *encoding* :utf-8
  "Default encoding keyword. :utf-8 (Babel :text) unless a transfer encoding is given.")

(defvar *encoding-backend* nil
  "Optional current encoding-backend (default encoding / pad / strict).")

(defclass encoding-backend ()
  ((encoding :initarg :encoding :initform :utf-8 :accessor encoding-backend-encoding)
   (pad :initarg :pad :initform t :accessor encoding-backend-pad)
   (strict :initarg :strict :initform nil :accessor encoding-backend-strict))
  (:documentation "Encoder settings. :text family (default :utf-8) or RFC 4648 / QP / RLE.
   Not HTTP Content-Encoding."))

(defun make-encoding-backend (&key (encoding :utf-8) (pad t) strict)
  (make-instance 'encoding-backend
                 :encoding (normalize-encoding encoding)
                 :pad pad
                 :strict strict))

(defun normalize-encoding (encoding)
  (flet ((from-keyword (kw)
           (case kw
             ((:utf-8 :utf8) :utf-8)
             ((:ascii :us-ascii) :ascii)
             ((:iso-8859-1 :latin-1 :latin1) :iso-8859-1)
             ((:base64 :b64) :base64)
             ((:base64url :base64-url :urlsafe :base64-urlsafe) :base64url)
             ((:base32 :b32) :base32)
             ((:base32hex :base32-hex :b32hex) :base32hex)
             ((:base16 :hex :hexadecimal) :base16)
             ((:quoted-printable :qp) :quoted-printable)
             ((:rle :run-length) :rle)
             (otherwise kw))))
    (etypecase encoding
      (null :utf-8)
      (keyword (from-keyword encoding))
      (string
       (from-keyword (intern (string-upcase (string-trim '(#\Space #\Tab) encoding))
                             :keyword)))
      (symbol (from-keyword (intern (symbol-name encoding) :keyword))))))

(defun %resolved-encoding (encoding)
  (normalize-encoding
   (or encoding
       (and *encoding-backend* (encoding-backend-encoding *encoding-backend*))
       *encoding*)))

(defun %encoding-family (encoding)
  "Transfer encodings are RFC 4648 / QP / RLE. Everything else is a Babel charset."
  (case encoding
    ((:base64 :base64url :base32 :base32hex :base16) :rfc4648)
    (:quoted-printable :quoted-printable)
    (:rle :rle)
    (t :text)))

(defun %effective-pad (pad pad-p)
  (cond
    (pad-p pad)
    (*encoding-backend* (encoding-backend-pad *encoding-backend*))
    (t t)))

(defun %effective-strict (strict strict-p)
  (cond
    (strict-p strict)
    (*encoding-backend* (encoding-backend-strict *encoding-backend*))
    (t nil)))

(defun %babel-string-to-octets (string encoding)
  (handler-case
      (babel:string-to-octets string :encoding encoding)
    (error (e)
      (error 'encoding-transcode-error
             :encoding encoding
             :message (princ-to-string e)))))

(defun %babel-octets-to-string (octets encoding)
  (handler-case
      (babel:octets-to-string octets :encoding encoding)
    (error (e)
      (error 'encoding-transcode-error
             :encoding encoding
             :message (princ-to-string e)))))

(defun %emit (out stream)
  (cond
    ((null stream) out)
    ((stringp out) (write-string out stream) out)
    (t (write-sequence out stream) out)))

(defun %encode-text (value encoding stream)
  (unless (stringp value)
    (error 'encoding-encode-error
           :message (format nil "encode ~S wants a string, got ~S"
                            encoding (type-of value))))
  (%emit (%babel-string-to-octets value encoding) stream))

(defun %encode-transfer (value encoding pad pad-p columns columns-p stream)
  (let* ((octets (%as-octets value))
         (out (ecase (%encoding-family encoding)
                (:rfc4648
                 (%wrap-columns
                  (%encode-octets octets (or (find-rfc4648-spec encoding)
                                             (error 'encoding-unknown-encoding
                                                    :encoding encoding))
                                  :pad (%effective-pad pad pad-p))
                  columns))
                (:quoted-printable
                 (%encode-quoted-printable octets
                                           :columns (if columns-p columns 76)))
                (:rle (%encode-rle octets)))))
    (%emit out stream)))

(defgeneric encode (value &key encoding pad columns stream)
  (:documentation
   "Encode VALUE.
    :text family (:utf-8 default, plus any Babel charset): string → octets.
    RFC 4648 / QP: octets (or string via UTF-8) → text. RLE → octets."))

(defmethod encode (value &key (encoding nil) (pad t pad-p)
                           (columns nil columns-p) stream)
  (let ((enc (%resolved-encoding encoding)))
    (if (eq (%encoding-family enc) :text)
        (%encode-text value enc stream)
        (%encode-transfer value enc pad pad-p columns columns-p stream))))

(defmethod encode ((value string) &key (encoding nil) (pad t pad-p)
                                    (columns nil columns-p) stream)
  (let ((enc (%resolved-encoding encoding)))
    (if (eq (%encoding-family enc) :text)
        (%encode-text value enc stream)
        (%encode-transfer value enc pad pad-p columns columns-p stream))))

(defun %decode-text (source encoding)
  (etypecase source
    (string source)
    ((vector (unsigned-byte 8))
     (%babel-octets-to-string source encoding))
    (vector
     (%babel-octets-to-string (%as-octets source) encoding))
    (stream
     (%babel-octets-to-string
      (let ((out (make-array 0 :element-type '(unsigned-byte 8)
                             :adjustable t :fill-pointer 0)))
        (loop for b = (read-byte source nil nil)
              while b do (vector-push-extend b out))
        out)
      encoding))))

(defun %decode-transfer (source encoding pad pad-p strict strict-p)
  (ecase (%encoding-family encoding)
    (:rfc4648
     (%decode-string (%as-ascii-string source)
                     (or (find-rfc4648-spec encoding)
                         (error 'encoding-unknown-encoding :encoding encoding))
                     :pad (%effective-pad pad pad-p)
                     :strict (%effective-strict strict strict-p)))
    (:quoted-printable
     (%decode-quoted-printable (%as-ascii-string source)))
    (:rle
     (%decode-rle (%as-octets source)))))

(defgeneric decode (source &key encoding pad strict)
  (:documentation
   "Decode SOURCE.
    :text family (:utf-8 default): octets → string (string source is identity).
    RFC 4648 / QP / RLE: text → octets."))

(defmethod decode (source &key (encoding nil) (pad t pad-p) (strict nil strict-p))
  (let ((enc (%resolved-encoding encoding)))
    (if (eq (%encoding-family enc) :text)
        (%decode-text source enc)
        (%decode-transfer source enc pad pad-p strict strict-p))))

(defun encode-to-octets (value &key encoding (pad t pad-p) columns)
  (let ((encoded (apply #'encode value :encoding encoding
                        (append (when pad-p (list :pad pad))
                                (when columns (list :columns columns))))))
    (if (and (vectorp encoded) (not (stringp encoded)))
        encoded
        (encode encoded))))

(defun decode-octets (octets &key encoding (pad t pad-p) (strict nil strict-p))
  (apply #'decode octets :encoding encoding
         (append (when pad-p (list :pad pad))
                 (when strict-p (list :strict strict)))))

(defun use-encoding-backend (&key (encoding :utf-8) (pad t) strict)
  (let ((backend (make-encoding-backend :encoding encoding :pad pad :strict strict)))
    (setf *encoding-backend* backend
          *encoding* (encoding-backend-encoding backend))
    backend))
