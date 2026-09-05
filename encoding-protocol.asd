(defsystem "encoding-protocol"
  :version "0.1.2"
  :description "Transfer encodings for cl-stack (RFC 4648, quoted-printable, RLE)"
  :author "egao1980"
  :license "MIT"
  :depends-on ("babel")
  :properties (:cl-repo (:ci (:sources (("serdes-protocol" :oci)))))
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "conditions")
               (:file "codec")
               (:file "rle")
               (:file "quoted-printable")
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
               (:file "serdes-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
