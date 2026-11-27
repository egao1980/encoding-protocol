(defsystem "encoding-protocol"
  :version "0.3.1"
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
               (:file "protocol"))
  :in-order-to ((test-op (test-op "encoding-protocol/tests"))))

(defsystem "encoding-protocol/serdes"
  :depends-on ("encoding-protocol" "serdes-protocol")
  :serial t
  :pathname "src"
  :components ((:file "serdes")))

(defsystem "encoding-protocol/tests"
  :depends-on ("encoding-protocol" "encoding-protocol/serdes" "serdes-protocol" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "codec-test")
               (:file "percent-test")
               (:file "charset-test")
               (:file "serdes-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
