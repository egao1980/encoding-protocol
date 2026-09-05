(in-package #:encoding-protocol/tests)

(deftest serdes-formats
  (dolist (fmt '(:base64 :base64url :base32 :base32hex :base16 :hex
                 :quoted-printable :rle))
    (ok (serdes-protocol:find-backend fmt) (string fmt))))

(deftest serdes-base64-roundtrip
  (let* ((octets (utf8 "foobar"))
         (text (serdes-protocol:encode octets :format :base64))
         (back (serdes-protocol:decode text :format :base64)))
    (ok (string= "Zm9vYmFy" text))
    (ok (equalp octets back))))

(deftest serdes-base32-roundtrip
  (let* ((octets (utf8 "foo"))
         (text (serdes-protocol:encode octets :format :base32)))
    (ok (string= "MZXW6===" text))
    (ok (equalp octets (serdes-protocol:decode text :format :base32)))))

(deftest encode-to-octets
  (let ((wire (encode-to-octets (utf8 "f") :encoding :base64)))
    (ok (equalp (babel:string-to-octets "Zg==" :encoding :ascii) wire))
    (ok (equalp (utf8 "f") (decode-octets wire :encoding :base64)))))

(deftest serdes-rle-roundtrip
  (let* ((raw (coerce #(9 9 8) '(vector (unsigned-byte 8))))
         (wire (serdes-protocol:encode raw :format :rle))
         (back (serdes-protocol:decode wire :format :rle)))
    (ok (equalp #(2 9 1 8) wire))
    (ok (equalp raw back))))
