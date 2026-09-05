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
             (otherwise kw))))
    (etypecase encoding
      (null :base64)
      (keyword (from-keyword encoding))
      (string
       (from-keyword (intern (string-upcase (string-trim '(#\Space #\Tab) encoding))
                             :keyword)))
      (symbol (from-keyword (intern (symbol-name encoding) :keyword))))))

(defun %effective-encoding (encoding)
  (find-rfc4648-spec
   (normalize-encoding
    (or encoding
        (and *encoding-backend* (encoding-backend-encoding *encoding-backend*))
        *encoding*))))

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

(defun encode (value &key (encoding nil encoding-p) (pad t pad-p) stream)
  "Octets or UTF-8 string → RFC 4648 text. ENCODING is :base64 (default),
   :base64url, :base32, :base32hex, or :base16. PAD defaults to T (canonical)."
  (declare (ignore encoding-p))
  (let* ((spec (%effective-encoding encoding))
         (text (%encode-octets (%as-octets value) spec
                               :pad (%effective-pad pad pad-p))))
    (if stream
        (progn (write-string text stream) text)
        text)))

(defun decode (source &key (encoding nil encoding-p) (pad t pad-p)
                        (strict nil strict-p))
  "RFC 4648 text (string, ASCII octets, or character stream) → octets.
   Whitespace is ignored unless STRICT. Missing pad is accepted unless STRICT
   and PAD are both true."
  (declare (ignore encoding-p))
  (%decode-string (%as-ascii-string source)
                  (%effective-encoding encoding)
                  :pad (%effective-pad pad pad-p)
                  :strict (%effective-strict strict strict-p)))

(defun encode-to-octets (value &key encoding (pad t pad-p))
  (babel:string-to-octets
   (apply #'encode value :encoding encoding (when pad-p (list :pad pad)))
   :encoding :ascii))

(defun decode-octets (octets &key encoding (pad t pad-p) (strict nil strict-p))
  (apply #'decode octets :encoding encoding
         (append (when pad-p (list :pad pad))
                 (when strict-p (list :strict strict)))))

(defun use-encoding-backend (&key (encoding :base64) (pad t) strict)
  (let ((backend (make-encoding-backend :encoding encoding :pad pad :strict strict)))
    (setf *encoding-backend* backend
          *encoding* (encoding-backend-encoding backend))
    backend))
