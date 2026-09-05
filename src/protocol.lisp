(in-package #:encoding-protocol)

(defvar *encoding* :base64
  "Default RFC 4648 encoding keyword.")

(defvar *encoding-backend* nil
  "Optional current encoding-backend (default encoding / pad / strict).")

(defclass encoding-backend ()
  ((encoding :initarg :encoding :initform :base64 :accessor encoding-backend-encoding)
   (pad :initarg :pad :initform t :accessor encoding-backend-pad)
   (strict :initarg :strict :initform nil :accessor encoding-backend-strict))
  (:documentation "RFC 4648 encoder settings. Not HTTP Content-Encoding."))

(defun make-encoding-backend (&key (encoding :base64) (pad t) strict)
  (make-instance 'encoding-backend
                 :encoding (normalize-encoding encoding)
                 :pad pad
                 :strict strict))

(defun normalize-encoding (encoding)
  (flet ((from-keyword (kw)
           (case kw
             ((:base64 :b64) :base64)
             ((:base64url :base64-url :urlsafe :base64-urlsafe) :base64url)
             ((:base32 :b32) :base32)
             ((:base32hex :base32-hex :b32hex) :base32hex)
             ((:base16 :hex :hexadecimal) :base16)
             ((:quoted-printable :qp) :quoted-printable)
             ((:rle :run-length) :rle)
             (otherwise kw))))
    (etypecase encoding
      (null :base64)
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
  (case encoding
    ((:base64 :base64url :base32 :base32hex :base16) :rfc4648)
    (:quoted-printable :quoted-printable)
    (:rle :rle)
    (otherwise
     (error 'encoding-unknown-encoding
            :encoding encoding
            :message "unknown encoding"))))

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

(defun encode (value &key (encoding nil encoding-p) (pad t pad-p)
                       (columns nil columns-p) stream)
  "Encode VALUE. :base64 / :base64url / :base32 / :base32hex / :base16 → text
   (PAD T is RFC-canonical; COLUMNS wraps with CRLF). :quoted-printable → text
   (COLUMNS default 76). :rle → octet vector (count,value pairs, count 1–255)."
  (declare (ignore encoding-p))
  (let* ((enc (%resolved-encoding encoding))
         (octets (%as-octets value))
         (out (ecase (%encoding-family enc)
                (:rfc4648
                 (%wrap-columns
                  (%encode-octets octets (or (find-rfc4648-spec enc)
                                             (error 'encoding-unknown-encoding
                                                    :encoding enc))
                                  :pad (%effective-pad pad pad-p))
                  columns))
                (:quoted-printable
                 (%encode-quoted-printable octets
                                           :columns (if columns-p columns 76)))
                (:rle (%encode-rle octets)))))
    (cond
      ((null stream) out)
      ((stringp out) (write-string out stream) out)
      (t (write-sequence out stream) out))))

(defun decode (source &key (encoding nil encoding-p) (pad t pad-p)
                        (strict nil strict-p))
  "Decode SOURCE → octets.
   RFC 4648: text / ASCII octets / character stream; whitespace ignored unless STRICT.
   :quoted-printable: same. :rle: octet vector of (count,value) pairs."
  (declare (ignore encoding-p))
  (let ((enc (%resolved-encoding encoding)))
    (ecase (%encoding-family enc)
      (:rfc4648
       (%decode-string (%as-ascii-string source)
                       (or (find-rfc4648-spec enc)
                           (error 'encoding-unknown-encoding :encoding enc))
                       :pad (%effective-pad pad pad-p)
                       :strict (%effective-strict strict strict-p)))
      (:quoted-printable
       (%decode-quoted-printable (%as-ascii-string source)))
      (:rle
       (%decode-rle (%as-octets source))))))

(defun encode-to-octets (value &key encoding (pad t pad-p) columns)
  (let ((encoded (apply #'encode value :encoding encoding
                        (append (when pad-p (list :pad pad))
                                (when columns (list :columns columns))))))
    (if (and (vectorp encoded) (not (stringp encoded)))
        encoded
        (babel:string-to-octets encoded :encoding :ascii))))

(defun decode-octets (octets &key encoding (pad t pad-p) (strict nil strict-p))
  (apply #'decode octets :encoding encoding
         (append (when pad-p (list :pad pad))
                 (when strict-p (list :strict strict)))))

(defun use-encoding-backend (&key (encoding :base64) (pad t) strict)
  (let ((backend (make-encoding-backend :encoding encoding :pad pad :strict strict)))
    (setf *encoding-backend* backend
          *encoding* (encoding-backend-encoding backend))
    backend))
