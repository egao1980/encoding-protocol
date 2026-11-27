(defpackage #:encoding-protocol
  (:use #:cl)
  (:nicknames #:stack-encoding)
  (:export #:encoding-error
           #:encoding-encode-error
           #:encoding-decode-error
           #:encoding-unknown-encoding
           #:encoding-transcode-error
           #:encoding-error-message
           #:encoding-unknown-encoding-encoding
           #:encoding-transcode-error-encoding
           #:encoding-decode-error-character

           #:encoding-backend
           #:encoding-backend-encoding
           #:encoding-backend-pad
           #:encoding-backend-strict
           #:*encoding-backend*
           #:*encoding*
           #:make-encoding-backend
           #:use-encoding-backend
           #:normalize-encoding

           #:encode
           #:decode
           #:encode-to-octets
           #:decode-octets
           #:rle-runs
           #:expand-rle-runs
           #:percent-encode
           #:percent-decode
           #:percent-decode-octets
           #:decode-utf-8-strict

           #:rfc4648-serdes-backend
           #:make-rfc4648-serdes-backend
           #:use-rfc4648-serdes-backend
           #:encoding-unknown-encoding-system
           #:encoding-unknown-encoding-load-error
           #:*auto-load-charsets*
           #:load-charsets
           #:load-charset-system))

(in-package #:encoding-protocol)
