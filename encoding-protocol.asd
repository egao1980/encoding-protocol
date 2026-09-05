(defsystem "encoding-protocol"
  :version "0.1.1"
  :description "Transfer encodings for cl-stack (RFC 4648, quoted-printable, RLE); serdes :base64 / :base32 / :base16 / :rle / :quoted-printable"
  :author "egao1980"
  :license "MIT"
  :depends-on ("babel" "serdes-protocol")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "conditions")
               (:file "codec")
               (:file "rle")
               (:file "quoted-printable")
               (:file "protocol")
               (:file "serdes"))
  :in-order-to ((test-op (test-op "encoding-protocol/tests"))))

(defsystem "encoding-protocol/tests"
  :depends-on ("encoding-protocol" "serdes-protocol" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "codec-test")
               (:file "serdes-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
