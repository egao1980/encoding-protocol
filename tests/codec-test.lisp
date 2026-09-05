(in-package #:encoding-protocol/tests)

;;; RFC 4648 §10 test vectors.

(deftest-parametrize rfc4648-base64
    ((plain b64)
     ("" "")
     ("f" "Zg==")
     ("fo" "Zm8=")
     ("foo" "Zm9v")
     ("foob" "Zm9vYg==")
     ("fooba" "Zm9vYmE=")
     ("foobar" "Zm9vYmFy"))
  (let ((octets (utf8 plain)))
    (ok (string= b64 (encode octets :encoding :base64)))
    (ok (equalp octets (decode b64 :encoding :base64)))))

(deftest-parametrize rfc4648-base32
    ((plain b32)
     ("" "")
     ("f" "MY======")
     ("fo" "MZXQ====")
     ("foo" "MZXW6===")
     ("foob" "MZXW6YQ=")
     ("fooba" "MZXW6YTB")
     ("foobar" "MZXW6YTBOI======"))
  (let ((octets (utf8 plain)))
    (ok (string= b32 (encode octets :encoding :base32)))
    (ok (equalp octets (decode b32 :encoding :base32)))
    (ok (equalp octets (decode (string-downcase b32) :encoding :base32)))))

(deftest-parametrize rfc4648-base32hex
    ((plain b32h)
     ("" "")
     ("f" "CO======")
     ("fo" "CPNG====")
     ("foo" "CPNMU===")
     ("foob" "CPNMUOG=")
     ("fooba" "CPNMUOJ1")
     ("foobar" "CPNMUOJ1E8======"))
  (let ((octets (utf8 plain)))
    (ok (string= b32h (encode octets :encoding :base32hex)))
    (ok (equalp octets (decode b32h :encoding :base32hex)))))

(deftest-parametrize rfc4648-base16
    ((plain hex)
     ("" "")
     ("f" "66")
     ("fo" "666F")
     ("foo" "666F6F")
     ("foob" "666F6F62")
     ("fooba" "666F6F6261")
     ("foobar" "666F6F626172"))
  (let ((octets (utf8 plain)))
    (ok (string= hex (encode octets :encoding :base16)))
    (ok (equalp octets (decode hex :encoding :base16)))
    (ok (equalp octets (decode (string-downcase hex) :encoding :hex)))))

(deftest base64url
  (let ((octets (coerce #(#xff #xef) '(vector (unsigned-byte 8)))))
    (ok (string= "/+8=" (encode octets :encoding :base64)))
    (ok (string= "_-8=" (encode octets :encoding :base64url)))
    (ok (string= "_-8" (encode octets :encoding :base64url :pad nil)))
    (ok (equalp octets (decode "_-8=" :encoding :base64url)))
    (ok (equalp octets (decode "_-8" :encoding :base64url :pad nil)))))

(deftest pad-optional
  (ok (string= "Zg" (encode (utf8 "f") :encoding :base64 :pad nil)))
  (ok (equalp (utf8 "f") (decode "Zg" :encoding :base64)))
  (ok (equalp (utf8 "f") (decode "Zg==" :encoding :base64 :strict t))))

(deftest whitespace
  (ok (equalp (utf8 "foo") (decode (format nil "Zm 9~Cv" #\Newline) :encoding :base64)))
  (ok (signals (decode (format nil "Zm 9v") :encoding :base64 :strict t)
               'encoding-decode-error)))

(deftest string-input-utf8
  (ok (string= "Zm9v" (encode "foo" :encoding :base64)))
  (ok (equalp (utf8 "foo") (decode (babel:string-to-octets "Zm9v" :encoding :ascii)
                                   :encoding :base64))))

(deftest unknown-encoding
  (ok (signals (encode #(1) :encoding :nope) 'encoding-unknown-encoding)))

(deftest wrap-columns
  (ok (string= (format nil "AB~C~CCD" #\Return #\Newline)
               (encode (coerce #(#xab #xcd) '(vector (unsigned-byte 8)))
                       :encoding :base16 :columns 2))))

(deftest rle-byte
  (let* ((raw (coerce #(1 1 1 2 2 3) '(vector (unsigned-byte 8))))
         (wire (encode raw :encoding :rle)))
    (ok (equalp #(3 1 2 2 1 3) wire))
    (ok (equalp raw (decode wire :encoding :rle))))
  (ok (equalp #() (encode #() :encoding :rle)))
  (let* ((long (make-array 300 :element-type '(unsigned-byte 8) :initial-element 7))
         (wire (encode long :encoding :rle)))
    (ok (equalp #(255 7 45 7) wire))
    (ok (equalp long (decode wire :encoding :rle)))))

(deftest rle-runs-api
  (ok (equal '((3 . 1) (2 . :x)) (rle-runs #(1 1 1 :x :x))))
  (ok (equalp #(1 1 1 :x :x) (expand-rle-runs '((3 . 1) (2 . :x))))))

(deftest quoted-printable
  (ok (string= "=3D" (encode #(61) :encoding :quoted-printable :columns nil)))
  (ok (equalp #(61) (decode "=3D" :encoding :quoted-printable)))
  (let* ((raw (babel:string-to-octets (format nil "a=~C~%b" #\Space) :encoding :utf-8))
         (qp (encode raw :encoding :qp))
         (back (decode qp :encoding :quoted-printable)))
    (ok (find #\= qp :test #'char=))
    (ok (equalp raw back))))

(deftest invalid-char-restarts
  (ok (signals (decode "Zg?=" :encoding :base64) 'encoding-decode-error))
  (let ((out (handler-bind ((encoding-decode-error
                             (lambda (c)
                               (declare (ignore c))
                               (continue))))
               (decode "Zg?=" :encoding :base64))))
    (ok (equalp (utf8 "f") out))))

(deftest normalize
  (ok (eq :base64 (normalize-encoding "b64")))
  (ok (eq :base64url (normalize-encoding :urlsafe)))
  (ok (eq :base16 (normalize-encoding :hex)))
  (ok (eq :base32hex (normalize-encoding "base32-hex"))))
