(in-package #:encoding-protocol/tests)

;;; Charsets Babel lacks (issue #7), registered with Babel by
;;; encoding-protocol (single-byte) and encoding-protocol/cjk (multibyte).
;;; Expected octets come from Python 3.12's codecs: inline rows below, and
;;; the exhaustive / sampled fixtures that tools/gen-charsets.py writes.

(defun fixture (name)
  (with-open-file (in (asdf:system-relative-pathname
                       "encoding-protocol"
                       (format nil "tests/fixtures/~A.sexp" name))
                      :external-format :utf-8)
    (with-standard-io-syntax
      (let ((*read-eval* nil)
            (*package* (find-package '#:encoding-protocol/tests)))
        (loop for form = (read in nil in)
              until (eq form in)
              collect form)))))

(defvar *fixtures* (make-hash-table :test 'equal))

(defun fixture-entry (file codec)
  (let ((forms (or (gethash file *fixtures*)
                   (setf (gethash file *fixtures*) (fixture file)))))
    (or (cdr (assoc codec forms :test #'string=))
        (error "no fixture ~A in ~A" codec file))))

(defun cps-string (cps)
  (map 'string #'code-char cps))

(defun encodes-p (text name)
  "Octets of TEXT in NAME, or :ERROR on ENCODING-TRANSCODE-ERROR."
  (handler-case (encode text :encoding name)
    (encoding-transcode-error () :error)))

(defun decodes-p (octets name)
  (handler-case (decode (coerce octets '(vector (unsigned-byte 8))) :encoding name)
    (encoding-transcode-error () :error)))

(deftest-parametrize more-charset-round-trip
    ((name text expected)
     :ids ("iso-2022-jp" "iso-2022-jp-roman" "big5" "euc-kr" "euc-kr-make-up"
           "ks_c_5601-1987" "cp949" "windows-1250" "windows-1253" "windows-1254"
           "windows-1255" "windows-1256" "windows-1257" "windows-1258"
           "gb18030" "gb18030-four-byte" "cp866" "macintosh" "tis-620" "windows-874")
     ("iso-2022-jp" "日本語のメール ABC"
      '(27 36 66 70 124 75 92 56 108 36 78 37 97 33 60 37 107 27 40 66 32 65 66 67))
     ("ISO-2022-JP" "¥100‾ 円"
      '(27 40 74 92 27 40 66 49 48 48 27 40 74 126 27 40 66 32 27 36 66 49 95 27 40 66))
     ("big5" "中文測試，繁體字 ABC"
      '(164 164 164 229 180 250 184 213 161 65 193 99 197 233 166 114 32 65 66 67))
     ("euc-kr" "한국어 메일 ABC"
      '(199 209 177 185 190 238 32 184 222 192 207 32 65 66 67))
     ("euc-kr" "똠방각하"
      '(164 212 164 168 164 199 164 177 185 230 176 162 199 207))
     ("ks_c_5601-1987" "한국어 똠방각하"
      '(199 209 177 185 190 238 32 140 99 185 230 176 162 199 207))
     ("cp949" "똠 ABC" '(140 99 32 65 66 67))
     ("windows-1250" "Příliš žluťoučký kůň €"
      '(80 248 237 108 105 154 32 158 108 117 157 111 117 232 107 253 32 107 249 242 32 128))
     ("windows-1253" "Καλημέρα κόσμε"
      '(202 225 235 231 236 221 241 225 32 234 252 243 236 229))
     ("windows-1254" "Günaydın dünya ğış"
      '(71 252 110 97 121 100 253 110 32 100 252 110 121 97 32 240 253 254))
     ("windows-1255" "שלום עולם ₪"
      '(249 236 229 237 32 242 229 236 237 32 164))
     ("windows-1256" "مرحبا بالعالم"
      '(227 209 205 200 199 32 200 199 225 218 199 225 227))
     ("windows-1257" "Labdien pasaule ąčęė"
      '(76 97 98 100 105 101 110 32 112 97 115 97 117 108 101 32 224 232 230 235))
     ("windows-1258" "Xin chào đ ₫ Ơ"
      '(88 105 110 32 99 104 224 111 32 240 32 254 32 213))
     ("gb18030" "中文测试€ 😀 ABC"
      '(214 208 206 196 178 226 202 212 162 227 32 148 57 252 54 32 65 66 67))
     ("gb18030" "ḿ·ก" '(129 53 244 55 161 164 129 50 206 57))
     ("cp866" "Привет, мир! ░▒▓"
      '(143 224 168 162 165 226 44 32 172 168 224 33 32 176 177 178))
     ("macintosh" "Café “quoted” ©™"
      '(67 97 102 142 32 210 113 117 111 116 101 100 211 32 169 170))
     ("tis-620" "สวัสดีชาวโลก"
      '(202 199 209 202 180 213 170 210 199 226 197 161))
     ("windows-874" "สวัสดี…€" '(202 199 209 202 180 213 133 128)))
  (let ((bytes (apply #'octets expected)))
    (ok (equalp bytes (encode text :encoding name)))
    (ok (string= text (decode bytes :encoding name)))))

;;; Every octet and every BMP code point of each single-byte table.
(deftest-parametrize single-byte-exhaustive
    ((codec)
     :ids ("cp1250" "cp1253" "cp1254" "cp1255" "cp1256" "cp1257" "cp1258"
           "cp866" "mac_roman" "tis_620" "cp874")
     ("cp1250") ("cp1253") ("cp1254") ("cp1255") ("cp1256") ("cp1257") ("cp1258")
     ("cp866") ("mac_roman") ("tis_620") ("cp874"))
  (destructuring-bind (decoded pairs) (fixture-entry "single-byte" codec)
    (let ((expected (make-hash-table)))
      (loop for (cp . octet) in pairs do (setf (gethash cp expected) octet))
      (ok (loop for octet from 0 below 256
                for want = (aref decoded octet)
                always (equal (decodes-p (list octet) codec)
                              (if want (string (code-char want)) :error))))
      (ok (loop for cp from 0 below #x10000
                      for want = (if (< cp #x80) cp (gethash cp expected))
                      always (or (<= #xd800 cp #xdfff)
                                 (equalp (encodes-p (string (code-char cp)) codec)
                                         (if want (octets want) :error))))))))

;;; A broad sample of each multibyte table, as Python encodes it.
(deftest-parametrize multibyte-sample
    ((codec)
     :ids ("iso2022_jp" "big5" "cp949" "euc_kr" "gb18030")
     ("iso2022_jp") ("big5") ("cp949") ("euc_kr") ("gb18030"))
  (let ((entries (fixture-entry "multibyte" codec)))
    (ok (< 20 (count :round-trip entries :key #'first)))
    (loop for (kind . args) in entries
          do (ecase kind
               (:round-trip
                (destructuring-bind (cps bytes) args
                  (let ((text (cps-string cps)))
                    (ok (equalp (apply #'octets bytes) (encode text :encoding codec)))
                    (ok (string= text (decode (apply #'octets bytes) :encoding codec))))))
               (:unencodable
                (ok (every (lambda (cp)
                             (eq :error (encodes-p (cps-string (list #x41 cp)) codec)))
                           args)))
               (:invalid
                (ok (every (lambda (bytes) (eq :error (decodes-p bytes codec)))
                           args)))))))

(deftest iso-2022-jp-mode-switching
  (testing "each run of JIS X 0208 is opened with ESC $ B and closed with ESC ( B"
    (ok (equalp (octets 65 27 36 66 70 124 27 40 66 10 27 36 66 70 124 27 40 66)
                (encode (format nil "A日~%日") :encoding "iso-2022-jp"))))
  (testing "JIS X 0201 Roman for ¥ and ‾, back to ASCII for the rest"
    (ok (equalp (octets 27 40 74 92 92 27 40 66 97)
                (encode "¥¥a" :encoding "iso-2022-jp"))))
  (testing "encode always ends in ASCII mode"
    (ok (equalp (octets 27 36 66 70 124 27 40 66) (encode "日" :encoding "iso-2022-jp")))
    (ok (equalp (octets 27 40 74 126 27 40 66) (encode "‾" :encoding "iso-2022-jp"))))
  (testing "decode: ESC $ @, ESC ( J, ESC $ ( B, control octets inside JIS X 0208"
    (ok (string= "日¥‾" (decode (octets 27 36 64 70 124 27 40 74 92 126) :encoding "iso-2022-jp")))
    (ok (string= "日A" (decode (octets 27 36 40 66 70 124 27 40 74 65) :encoding "iso-2022-jp")))
    (ok (string= (format nil "日~%日")
                 (decode (octets 27 36 66 70 124 10 70 124 27 40 66) :encoding "iso-2022-jp")))
    (ok (string= "日" (decode (octets 27 36 66 70 124) :encoding "iso-2022-jp"))))
  (testing "Python's literal pass-through of a non-ISO 2022 escape"
    (ok (string= (cps-string '(27 78 97)) (decode (octets 27 78 97) :encoding "iso-2022-jp"))))
  (testing "malformed escapes and octets"
    (dolist (bytes '((27) (27 40) (27 36) (27 40 73 49) (27 36 65) (27 36 66 70)
                     (27 36 66 32 33) (128) (27 40 74 128)))
      (ok (eq :error (decodes-p bytes "iso-2022-jp"))))))

(deftest-parametrize more-charset-unencodable
    ((name char)
     :ids ("iso-2022-jp-katakana" "big5-hangul" "euc-kr-simplified" "cp949-simplified"
           "windows-1250-cyrillic" "cp866-euro" "macintosh-cyrillic" "tis-620-euro")
     ("iso-2022-jp" #.(code-char #xff76))
     ("big5" #.(code-char #xd55c))
     ("euc-kr" #.(code-char #x6d4b))
     ("cp949" #.(code-char #x6d4b))
     ("windows-1250" #.(code-char #x0416))
     ("cp866" #.(code-char #x20ac))
     ("macintosh" #.(code-char #x0416))
     ("tis-620" #.(code-char #x20ac)))
  (let ((text (concatenate 'string "ab" (string char))))
    (ok (signals (encode text :encoding name) 'encoding-transcode-error))
    (handler-case (encode text :encoding name)
      (encoding-transcode-error (e)
        (let ((message (princ-to-string e)))
          (ok (search (format nil "U+~4,'0X" (char-code char)) message))
          (ok (search "index 2" message)))))))

(deftest-parametrize more-charset-truncated
    ((name bytes)
     :ids ("iso-2022-jp" "big5" "euc-kr" "euc-kr-make-up" "cp949" "gb18030-two"
           "gb18030-four" "gb18030-three" "windows-1253-unmapped" "cp874-unmapped")
     ("iso-2022-jp" '(27 36 66 70))
     ("big5" '(65 164))
     ("euc-kr" '(199))
     ("euc-kr" '(164 212 164 168 164 199))
     ("cp949" '(140))
     ("gb18030" '(214))
     ("gb18030" '(129 48))
     ("gb18030" '(129 48 129))
     ("windows-1253" '(170))
     ("cp874" '(219)))
  (ok (signals (decode (apply #'octets bytes) :encoding name)
        'encoding-transcode-error)))

(deftest-parametrize more-charset-alias
    ((name canonical)
     ("iso-2022-jp" :iso-2022-jp) ("csISO2022JP" :iso-2022-jp) ("iso2022_jp" :iso-2022-jp)
     ("big5" :big5) ("Big5" :big5) ("big5-tw" :big5) ("csbig5" :big5) ("cn-big5" :big5)
     ("x-x-big5" :big5)
     ("euc-kr" :euc-kr) ("EUC-KR" :euc-kr) ("euckr" :euc-kr) ("cseuckr" :euc-kr)
     ("cp949" :cp949) ("ks_c_5601-1987" :cp949) ("ks_c_5601" :cp949) ("uhc" :cp949)
     ("windows-949" :cp949) ("korean" :cp949) ("ms949" :cp949) ("csksc56011987" :cp949)
     ("windows-1250" :cp1250) ("cp1250" :cp1250) ("x-cp1250" :cp1250)
     ("windows-1253" :cp1253) ("windows-1254" :cp1254) ("windows-1255" :cp1255)
     ("windows-1256" :cp1256) ("windows-1257" :cp1257) ("windows-1258" :cp1258)
     ("gb18030" :gb18030) ("GB18030-2000" :gb18030)
     ("cp866" :cp866) ("ibm866" :cp866) ("csibm866" :cp866) ("866" :cp866)
     ("mac-roman" :mac-roman) ("macintosh" :mac-roman) ("macroman" :mac-roman)
     ("csmacintosh" :mac-roman) ("x-mac-roman" :mac-roman)
     ("tis-620" :tis-620) ("tis620" :tis-620) ("TIS-620-0" :tis-620) ("iso-ir-166" :tis-620)
     ("cp874" :cp874) ("windows-874" :cp874) ("ms874" :cp874) (:windows-874 :cp874))
  (ok (eq canonical (normalize-encoding name)))
  (ok (null (nth-value 1 (normalize-encoding name))))
  (progn
    (ok (equalp (octets 97) (encode "a" :encoding name)))
    ;; Babel encodes a SIMPLE-BASE-STRING with separate mappings on SBCL.
    (ok (equalp (octets 97 98) (encode (coerce "ab" 'simple-base-string) :encoding name)))))

(deftest optional-charset-system
  (testing "with *auto-load-charsets* NIL, a charset from a system that is not loaded names that system"
    (let ((encoding-protocol::*optional-charsets*
            (acons :x-test-charset "encoding-protocol/x-test"
                   encoding-protocol::*optional-charsets*))
          (*auto-load-charsets* nil))
      (ok (equal "encoding-protocol/x-test"
                 (nth-value 1 (normalize-encoding "x-test-charset"))))
      (ok (signals (encode "a" :encoding "x-test-charset") 'encoding-unknown-encoding))
      (handler-case (decode (octets 97) :encoding "x-test-charset")
        (encoding-unknown-encoding (e)
          (ok (equal "encoding-protocol/x-test" (encoding-unknown-encoding-system e)))
          (ok (null (encoding-unknown-encoding-load-error e)))
          (ok (search "encoding-protocol/x-test" (princ-to-string e)))))))
  (testing "auto-loading a system ASDF cannot find signals the typed condition"
    (let ((encoding-protocol::*optional-charsets*
            (acons :x-test-charset "encoding-protocol/x-test"
                   encoding-protocol::*optional-charsets*)))
      (handler-case (encode "a" :encoding "x-test-charset")
        (encoding-unknown-encoding (e)
          (ok (equal "encoding-protocol/x-test" (encoding-unknown-encoding-system e)))
          (ok (typep (encoding-unknown-encoding-load-error e) 'asdf:missing-component))
          (ok (search "loading \"encoding-protocol/x-test\" failed" (princ-to-string e)))))
      (ok (signals (normalize-encoding "x-test-charset") 'encoding-unknown-encoding))
      (ok (equal '(:x-test-charset "encoding-protocol/x-test")
                 (handler-bind ((encoding-unknown-encoding #'continue))
                   (multiple-value-list (normalize-encoding "x-test-charset")))))
      (ok (equalp (octets 97)
                  (handler-bind ((encoding-unknown-encoding
                                   (lambda (e) (use-value :ascii e))))
                    (encode "a" :encoding "x-test-charset"))))))
  (testing "a charset with no providing system has no system"
    (ok (null (nth-value 1 (normalize-encoding "x-no-such-charset"))))
    (handler-case (encode "a" :encoding "x-no-such-charset")
      (encoding-unknown-encoding (e)
        (ok (null (encoding-unknown-encoding-system e)))
        (ok (null (encoding-unknown-encoding-load-error e)))))))
