(in-package #:encoding-protocol)

;;; Single-byte charsets Babel lacks (issue #7). Tables: single-byte-tables.lisp.

(define-single-byte-charset :cp1250 *cp1250-table*
  (:aliases (:windows-1250)
   :documentation "Windows-1250: Central European (Python cp1250)."))

(define-single-byte-charset :cp1253 *cp1253-table*
  (:aliases (:windows-1253)
   :documentation "Windows-1253: Greek (Python cp1253)."))

(define-single-byte-charset :cp1254 *cp1254-table*
  (:aliases (:windows-1254)
   :documentation "Windows-1254: Turkish (Python cp1254)."))

(define-single-byte-charset :cp1255 *cp1255-table*
  (:aliases (:windows-1255)
   :documentation "Windows-1255: Hebrew (Python cp1255)."))

(define-single-byte-charset :cp1256 *cp1256-table*
  (:aliases (:windows-1256)
   :documentation "Windows-1256: Arabic (Python cp1256)."))

(define-single-byte-charset :cp1257 *cp1257-table*
  (:aliases (:windows-1257)
   :documentation "Windows-1257: Baltic (Python cp1257)."))

(define-single-byte-charset :cp1258 *cp1258-table*
  (:aliases (:windows-1258)
   :documentation "Windows-1258: Vietnamese, without composition (Python cp1258)."))

(define-single-byte-charset :cp866 *cp866-table*
  (:aliases (:ibm866)
   :documentation "IBM / DOS code page 866: Cyrillic (Python cp866)."))

(define-single-byte-charset :mac-roman *mac-roman-table*
  (:aliases (:macintosh)
   :documentation "Mac OS Roman (Python mac_roman)."))

(define-single-byte-charset :tis-620 *tis-620-table*
  (:aliases ()
   :documentation "TIS-620: Thai (Python tis_620)."))

(define-single-byte-charset :cp874 *cp874-table*
  (:aliases (:windows-874)
   :documentation "Windows-874: Thai (Python cp874)."))

(register-babel-charsets :cp1250 :cp1253 :cp1254 :cp1255 :cp1256 :cp1257 :cp1258
                         :cp866 :mac-roman :tis-620 :cp874)
