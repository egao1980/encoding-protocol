(defsystem "encoding-protocol"
  :version "0.4.0"
  :description "Text (UTF-8 default) + transfer encodings for cl-stack (RFC 4648, QP, RLE, RFC 3986 percent)"
  :author "egao1980"
  :license "MIT"
  :depends-on ("babel")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "conditions")
               (:file "codec")
               (:file "rle")
               (:file "quoted-printable")
               (:file "percent")
               (:file "protocol")
               (:file "charset-engine")
               (:file "single-byte-tables")
               (:file "single-byte-charsets"))
  :in-order-to ((test-op (test-op "encoding-protocol/tests"))))

(defsystem "encoding-protocol/cjk"
  :description "ISO-2022-JP, Big5, EUC-KR, CP949 and GB18030 for encoding-protocol (Babel encodings)"
  :depends-on ("encoding-protocol")
  :serial t
  :pathname "src/cjk"
  :components ((:file "jisx0208-table")
               (:file "big5-table")
               (:file "euc-kr-table")
               (:file "cp949-table")
               (:file "gb18030-table")
               (:file "cjk")))

(defsystem "encoding-protocol/serdes"
  :depends-on ("encoding-protocol" "serdes-protocol")
  :serial t
  :pathname "src"
  :components ((:file "serdes")))

(defsystem "encoding-protocol/tests"
  :depends-on ("encoding-protocol" "encoding-protocol/cjk" "encoding-protocol/serdes"
               "serdes-protocol" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "codec-test")
               (:file "percent-test")
               (:file "charset-test")
               (:file "more-charsets-test")
               (:file "serdes-test")
               (:static-file "fixtures/single-byte.sexp")
               (:static-file "fixtures/multibyte.sexp"))
  :perform (test-op (o c)
                    (unless (symbol-call :rove :run c)
                      (error "tests failed for ~A" (component-name c)))))
