(in-package #:encoding-protocol)

(define-condition encoding-error (error)
  ((message :initarg :message :reader encoding-error-message :initform nil))
  (:report (lambda (c s)
             (format s "Encoding error~@[: ~A~]" (encoding-error-message c)))))

(define-condition encoding-encode-error (encoding-error) ())

(define-condition encoding-decode-error (encoding-error)
  ((character :initarg :character :reader encoding-decode-error-character :initform nil)))

(define-condition encoding-unknown-encoding (encoding-error)
  ((encoding :initarg :encoding :reader encoding-unknown-encoding-encoding)
   (system :initarg :system :initform nil :reader encoding-unknown-encoding-system
           :documentation "ASDF system that provides ENCODING, when one does.")
   (load-error :initarg :load-error :initform nil
               :reader encoding-unknown-encoding-load-error
               :documentation "The condition from loading SYSTEM, when that failed."))
  (:report (lambda (c s)
             (format s "Unknown encoding ~S~@[: ~A~]"
                     (encoding-unknown-encoding-encoding c)
                     (encoding-error-message c))
             (let ((system (encoding-unknown-encoding-system c))
                   (load-error (encoding-unknown-encoding-load-error c)))
               (cond (load-error (format s " (loading ~S failed: ~A)" system load-error))
                     (system (format s " (load the ASDF system ~S)" system)))))))

(define-condition encoding-transcode-error (encoding-error)
  ((encoding :initarg :encoding :reader encoding-transcode-error-encoding :initform nil))
  (:report (lambda (c s)
             (format s "Transcode error~@[ (~S)~]~@[: ~A~]"
                     (encoding-transcode-error-encoding c)
                     (encoding-error-message c)))))
