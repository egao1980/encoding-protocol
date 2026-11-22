(in-package #:encoding-protocol/tests)

;;; RFC 3986 percent-encoding and strict UTF-8 (RFC 3629, Unicode Table 3-7).

(deftest-parametrize percent-encode-cases
    ((input safe expected)
     ("abcXYZ019-._~" "" "abcXYZ019-._~")
     ("a b/c" "" "a%20b%2Fc")
     ("a b/c" "/" "a%20b/c")
     ("é" "" "%C3%A9")
     ("ж/日本" "/" "%D0%B6/%E6%97%A5%E6%9C%AC")
     ("😀" "" "%F0%9F%98%80")
     ("*+=&" "" "%2A%2B%3D%26"))
  (ok (string= expected (percent-encode input :safe safe))))

(deftest percent-encode-octets-and-plus
  (ok (string= "%00%FF" (percent-encode (coerce #(0 255) '(vector (unsigned-byte 8))))))
  (ok (string= "a+b" (percent-encode "a b" :space-as-plus t))))

(deftest percent-roundtrip
  (dolist (s '("" "plain" "a b/c?d=e&f" "é ж 日本 😀" "%"))
    (ok (string= s (percent-decode (percent-encode s))))))

(deftest percent-decode-cases
  (ok (string= "a/b" (percent-decode "a%2fb")))
  (ok (string= "é" (percent-decode "%c3%a9")))
  (ok (string= "a b" (percent-decode "a+b" :plus-as-space t)))
  (ok (string= "a+b" (percent-decode "a+b")))
  (ok (equalp (coerce #(255 0) '(simple-array (unsigned-byte 8) (*)))
              (percent-decode-octets "%FF%00"))))

(deftest-parametrize percent-decode-malformed-escape
    ((input)
     ("%") ("%4") ("%G0") ("a%2") ("ж"))
  (ok (signals (percent-decode input) 'encoding-decode-error)))

(deftest-parametrize percent-decode-invalid-utf-8
    ((input)
     ("%C0%AF")                 ; overlong "/"
     ("%C1%BF")                 ; overlong
     ("%E0%80%AF")              ; 3-byte overlong "/"
     ("%F0%80%80%AF")           ; 4-byte overlong "/"
     ("%ED%A0%80")              ; surrogate U+D800
     ("%ED%BF%BF")              ; surrogate U+DFFF
     ("%F4%90%80%80")           ; > U+10FFFF
     ("%F5%80%80%80")           ; invalid lead
     ("%FF")                    ; invalid lead
     ("%80")                    ; stray continuation
     ("%C3")                    ; truncated
     ("%E6%97")                 ; truncated
     ("%C3%28"))                ; bad continuation
  (ok (signals (percent-decode input) 'encoding-decode-error)))

(deftest strict-utf-8-decode
  (ok (string= "é😀" (decode (utf8 "é😀") :strict t)))
  (ok (string= "é😀" (decode-utf-8-strict (utf8 "é😀"))))
  (ok (string= (string (code-char #x10FFFF))
               (decode-utf-8-strict (coerce #(#xF4 #x8F #xBF #xBF) '(vector (unsigned-byte 8))))))
  (ok (signals (decode (coerce #(#xC0 #xAF) '(vector (unsigned-byte 8))) :strict t)
               'encoding-decode-error)))
