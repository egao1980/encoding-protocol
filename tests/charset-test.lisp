(in-package #:encoding-protocol/tests)

;;; Charsets beyond UTF-8 (issue #5). Expected octets come from
;;; Python's codecs (`"…".encode(name)`).

(defun octets (&rest bytes)
  (coerce bytes '(vector (unsigned-byte 8))))

(deftest-parametrize charset-round-trip
    ((name text expected)
     :ids ("shift_jis" "sjis" "windows-31j" "euc-jp" "gb2312" "gbk" "cp936"
           "koi8-r" "koi8-u")
     ("shift_jis" "日本語テキスト、ｶﾀｶﾅ ABC"
      '(147 250 150 123 140 234 131 101 131 76 131 88 131 103 129 65 182 192 182 197 32 65 66 67))
     ("SJIS" "日本語" '(147 250 150 123 140 234))
     ("Windows-31J" "日本語 ①" '(147 250 150 123 140 234 32 135 64))
     ("EUC-JP" "日本語テキスト、ｶﾀｶﾅ ABC"
      '(198 252 203 220 184 236 165 198 165 173 165 185 165 200 161 162 142 182 142 192 142 182 142 197 32 65 66 67))
     ("gb2312" "中文测试，汉字 ABC"
      '(214 208 206 196 178 226 202 212 163 172 186 186 215 214 32 65 66 67))
     ("GBK" "中文測試 ABC" '(214 208 206 196 156 121 212 135 32 65 66 67))
     ("cp936" "中文" '(214 208 206 196))
     ("koi8-r" "Привет, мир!" '(240 210 201 215 197 212 44 32 205 201 210 33))
     ("KOI8-U" "Привіт, світ! Ґ"
      '(240 210 201 215 166 212 44 32 211 215 166 212 33 32 189)))
  (let ((bytes (apply #'octets expected)))
    (ok (equalp bytes (encode text :encoding name)))
    (ok (string= text (decode bytes :encoding name)))))

(deftest-parametrize charset-alias
    ((name canonical)
     ("shift_jis" :cp932) ("Shift-JIS" :cp932) ("x-sjis" :cp932) ("ms_kanji" :cp932)
     ("euc-jp" :eucjp) ("x-euc-jp" :eucjp) ("ujis" :eucjp)
     ("gb2312" :gbk) ("euc-cn" :gbk) ("ms936" :gbk) ("csGB2312" :gbk)
     ("csKOI8R" :koi8-r) ("latin2" :iso-8859-2) ("ISO_8859-1" :iso-8859-1)
     ("windows-1251" :cp1251) ("utf8" :utf-8) (:shift_jis :cp932))
  (ok (eq canonical (normalize-encoding name))))

(deftest-parametrize charset-unencodable
    ((name char)
     :ids ("koi8-r-e-acute" "koi8-r-euro" "shift_jis-euro" "euc-jp-euro" "gb2312-euro")
     ("koi8-r" #.(code-char #xe9))
     ("koi8-r" #.(code-char #x20ac))
     ("shift_jis" #.(code-char #x20ac))
     ("euc-jp" #.(code-char #x20ac))
     ("gb2312" #.(code-char #x20ac)))
  (let ((text (concatenate 'string "ab" (string char))))
    (ok (signals (encode text :encoding name) 'encoding-transcode-error))
    (handler-case (encode text :encoding name)
      (encoding-transcode-error (e)
        (let ((message (princ-to-string e)))
          (ok (search (format nil "U+~4,'0X" (char-code char)) message))
          (ok (search "index 2" message))
          (ng (search "condition class" message)))))))

(deftest-parametrize charset-undecodable
    ((name bytes)
     :ids ("shift_jis-truncated" "euc-jp-truncated" "gbk-truncated")
     ("shift_jis" '(#x93))
     ("euc-jp" '(#xa1))
     ("gbk" '(#x81)))
  (ok (signals (decode (apply #'octets bytes) :encoding name)
        'encoding-transcode-error)))

(deftest-parametrize charset-unknown
    ((name)
     :ids ("big5" "iso-2022-jp" "nonsense" "keyword")
     ("big5") ("iso-2022-jp") ("x-no-such-charset") (:big5))
  (ok (signals (encode "abc" :encoding name) 'encoding-unknown-encoding))
  (ok (signals (decode (octets 97 98) :encoding name) 'encoding-unknown-encoding))
  (handler-case (encode "abc" :encoding name)
    (encoding-unknown-encoding (e)
      (ok (eq (normalize-encoding name) (encoding-unknown-encoding-encoding e)))
      (ok (typep e 'encoding-error)))))

(deftest charset-unknown-use-value
  (ok (string= "ab"
               (handler-bind ((encoding-unknown-encoding
                                (lambda (c) (use-value :iso-8859-1 c))))
                 (decode (octets 97 98) :encoding "big5")))))
