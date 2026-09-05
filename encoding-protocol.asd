(defsystem "encoding-protocol"
  :version "0.1.0"
  :description "RFC 4648 Base16/32/64 encode/decode for cl-stack; implements serdes-protocol :base64 / :base64url / :base32 / :base32hex / :base16"
  :author "egao1980"
  :license "MIT"
  :depends-on ("babel" "serdes-protocol")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "conditions")
               (:file "codec")
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
