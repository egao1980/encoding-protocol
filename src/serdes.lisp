(in-package #:encoding-protocol)

(defclass rfc4648-serdes-backend (serdes-protocol:serdes-backend encoding-backend) ()
  (:documentation "serdes-protocol implementor for RFC 4648 encodings."))

(defun make-rfc4648-serdes-backend (&key (encoding :base64) (pad t) strict)
  (make-instance 'rfc4648-serdes-backend
                 :encoding (normalize-encoding encoding)
                 :pad pad
                 :strict strict))

(defmethod serdes-protocol:backend-encode ((backend rfc4648-serdes-backend) value &key stream)
  (encode value :encoding (encoding-backend-encoding backend)
                :pad (encoding-backend-pad backend)
                :stream stream))

(defmethod serdes-protocol:backend-decode ((backend rfc4648-serdes-backend) source &key)
  (decode source :encoding (encoding-backend-encoding backend)
                 :pad (encoding-backend-pad backend)
                 :strict (encoding-backend-strict backend)))

(defmethod serdes-protocol:backend-make-input-stream ((backend rfc4648-serdes-backend)
                                                      underlying
                                                      &key (element-type 'character))
  (unless (subtypep element-type 'character)
    (error 'encoding-error
           :message (format nil "RFC 4648 streams are character, got ~S" element-type)))
  (make-instance 'serdes-protocol:serdes-character-input-stream
                 :underlying underlying
                 :backend backend))

(defmethod serdes-protocol:backend-make-output-stream ((backend rfc4648-serdes-backend)
                                                       underlying
                                                       &key (element-type 'character))
  (unless (subtypep element-type 'character)
    (error 'encoding-error
           :message (format nil "RFC 4648 streams are character, got ~S" element-type)))
  (make-instance 'serdes-protocol:serdes-character-output-stream
                 :underlying underlying
                 :backend backend))

(defmethod serdes-protocol:stream-encode-value
    ((stream serdes-protocol:serdes-character-output-stream) value &key)
  (let ((backend (serdes-protocol:stream-backend stream)))
    (unless (typep backend 'rfc4648-serdes-backend)
      (error 'encoding-encode-error :message "stream backend is not RFC 4648"))
    (encode value :encoding (encoding-backend-encoding backend)
                  :pad (encoding-backend-pad backend)
                  :stream (serdes-protocol:underlying-stream stream))))

(defmethod serdes-protocol:stream-decode-value
    ((stream serdes-protocol:serdes-character-input-stream) &key)
  (let ((backend (serdes-protocol:stream-backend stream)))
    (unless (typep backend 'rfc4648-serdes-backend)
      (error 'encoding-decode-error :message "stream backend is not RFC 4648"))
    (decode (serdes-protocol:underlying-stream stream)
            :encoding (encoding-backend-encoding backend)
            :pad (encoding-backend-pad backend)
            :strict (encoding-backend-strict backend))))

(defun use-rfc4648-serdes-backend (&key (encoding :base64) (pad t) strict)
  (let ((backend (make-rfc4648-serdes-backend :encoding encoding :pad pad :strict strict)))
    (setf *encoding-backend* backend
          *encoding* (encoding-backend-encoding backend))
    (serdes-protocol:register-format (encoding-backend-encoding backend) backend)
    backend))

(defun %install-formats ()
  (dolist (entry '((:base64 "application/base64")
                   (:base64url nil)
                   (:base32 nil)
                   (:base32hex nil)
                   (:base16 "text/plain")
                   (:hex "text/plain")))
    (destructuring-bind (format media) entry
      (let* ((encoding (normalize-encoding format))
             (backend (make-rfc4648-serdes-backend :encoding encoding)))
        (apply #'serdes-protocol:register-format format backend
               (when media (list :media-type media))))))
  (use-encoding-backend :encoding :base64))

(%install-formats)
