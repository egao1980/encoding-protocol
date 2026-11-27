(in-package #:encoding-protocol)

(defvar *encoding* :utf-8
  "Default encoding keyword. :utf-8 (Babel :text) unless a transfer encoding is given.")

(defvar *encoding-backend* nil
  "Optional current encoding-backend (default encoding / pad / strict).")

(defclass encoding-backend ()
  ((encoding :initarg :encoding :initform :utf-8 :accessor encoding-backend-encoding)
   (pad :initarg :pad :initform t :accessor encoding-backend-pad)
   (strict :initarg :strict :initform nil :accessor encoding-backend-strict))
  (:documentation "Encoder settings. :text family (default :utf-8) or RFC 4648 / QP / RLE.
   Not HTTP Content-Encoding."))

(defun make-encoding-backend (&key (encoding :utf-8) (pad t) strict)
  (make-instance 'encoding-backend
                 :encoding (normalize-encoding encoding)
                 :pad pad
                 :strict strict))

(defun %encoding-key (name)
  "Lookup key for an encoding NAME: upcased, letters and digits only
   (\"Shift_JIS\" and \"shift-jis\" → \"SHIFTJIS\")."
  (string-upcase (remove-if-not #'alphanumericp (string name))))

(defparameter *encoding-aliases*
  (let ((table (make-hash-table :test 'equal)))
    (loop for (target . names)
            in '(;; Transfer encodings
                 (:base64 "base64" "b64")
                 (:base64url "base64url" "urlsafe" "base64urlsafe")
                 (:base32 "base32" "b32")
                 (:base32hex "base32hex" "b32hex")
                 (:base16 "base16" "hex" "hexadecimal")
                 (:quoted-printable "quoted-printable" "qp")
                 (:rle "rle" "run-length")
                 ;; IANA charset names and common aliases (Python codecs) → Babel.
                 (:utf-8 "utf-8" "utf8" "u8" "cp65001")
                 (:ascii "us-ascii" "ascii" "ansi_x3.4-1968" "iso646-us" "us"
                         "cp367" "ibm367" "csascii")
                 (:iso-8859-1 "latin-1" "latin1" "l1" "cp819" "ibm819" "csisolatin1")
                 (:iso-8859-2 "latin2" "l2" "csisolatin2")
                 (:iso-8859-3 "latin3" "l3" "csisolatin3")
                 (:iso-8859-4 "latin4" "l4" "csisolatin4")
                 (:iso-8859-5 "cyrillic" "csisolatincyrillic")
                 (:iso-8859-6 "arabic" "csisolatinarabic")
                 (:iso-8859-7 "greek" "greek8" "csisolatingreek")
                 (:iso-8859-8 "hebrew" "csisolatinhebrew")
                 (:iso-8859-9 "latin5" "l5" "csisolatin5")
                 (:iso-8859-10 "latin6" "l6" "csisolatin6")
                 (:iso-8859-11 "thai")
                 (:iso-8859-13 "latin7" "l7")
                 (:iso-8859-14 "latin8" "l8")
                 (:iso-8859-15 "latin9" "l9")
                 (:iso-8859-16 "latin10" "l10")
                 (:cp1251 "windows-1251" "cp1251")
                 (:cp1252 "windows-1252" "cp1252")
                 (:koi8-r "koi8-r" "cskoi8r")
                 (:koi8-u "koi8-u")
                 (:koi8-ru "koi8-ru")
                 ;; Babel's CP932 is the Shift_JIS mail clients send (Windows-31J).
                 (:cp932 "shift_jis" "shift-jis" "sjis" "x-sjis" "ms_kanji"
                         "csshiftjis" "windows-31j" "cswindows31j" "cp932" "ms932" "mskanji")
                 (:eucjp "euc-jp" "eucjp" "x-euc-jp" "ujis" "u-jis"
                         "cseucpkdfmtjapanese")
                 ;; GBK is a superset of GB2312 (EUC-CN): same octets for GB2312 text.
                 (:gbk "gb2312" "csgb2312" "gb2312-80" "euc-cn" "euccn" "x-euc-cn"
                       "chinese" "gbk" "x-gbk" "cp936" "ms936" "windows-936"))
          do (dolist (name names)
               (setf (gethash (%encoding-key name) table) target)))
    table)
  "Encoding-key → canonical keyword (transfer encodings and charset aliases).")

(defvar *babel-encoding-names* nil
  "(count . table): Babel names and aliases by encoding key, rebuilt when the
   number of Babel encodings changes.")

(defun %babel-encoding-names ()
  (let ((names (babel-encodings:list-character-encodings)))
    (if (eql (car *babel-encoding-names*) (length names))
        (cdr *babel-encoding-names*)
        (let ((table (make-hash-table :test 'equal)))
          (dolist (name names)
            (dolist (alias (cons name (babel-encodings:enc-aliases
                                       (babel-encodings:get-character-encoding name))))
              (setf (gethash (%encoding-key alias) table) name)))
          (setf *babel-encoding-names* (cons (length names) table))
          table))))

(defun %babel-encoding-p (encoding)
  (and (keywordp encoding)
       (ignore-errors (babel-encodings:get-character-encoding encoding))
       t))

(defun normalize-encoding (encoding)
  "Canonical keyword for ENCODING (string, symbol or NIL).
   Names match case-insensitively and ignore punctuation, so \"Shift_JIS\",
   \"shift-jis\" and :SJIS are one name. Charset aliases resolve to the Babel
   encoding (\"shift_jis\" → :CP932, \"gb2312\" → :GBK). An unknown name comes
   back as a keyword; ENCODE / DECODE signal ENCODING-UNKNOWN-ENCODING for it."
  (flet ((resolve (name)
           (let ((key (%encoding-key name)))
             (or (gethash key *encoding-aliases*)
                 (and (string/= key "") (gethash key (%babel-encoding-names)))
                 (intern (string-upcase (string-trim '(#\Space #\Tab) name))
                         :keyword)))))
    (etypecase encoding
      (null :utf-8)
      (string (resolve encoding))
      (symbol (resolve (symbol-name encoding))))))

(defun %resolved-encoding (encoding)
  (normalize-encoding
   (or encoding
       (and *encoding-backend* (encoding-backend-encoding *encoding-backend*))
       *encoding*)))

(defun %encoding-family (encoding)
  "Transfer encodings are RFC 4648 / QP / RLE. Everything else is a Babel charset."
  (case encoding
    ((:base64 :base64url :base32 :base32hex :base16) :rfc4648)
    (:quoted-printable :quoted-printable)
    (:rle :rle)
    (t :text)))

(defun %effective-pad (pad pad-p)
  (cond
    (pad-p pad)
    (*encoding-backend* (encoding-backend-pad *encoding-backend*))
    (t t)))

(defun %effective-strict (strict strict-p)
  (cond
    (strict-p strict)
    (*encoding-backend* (encoding-backend-strict *encoding-backend*))
    (t nil)))

(defun %text-encoding (encoding)
  "ENCODING when Babel knows it, else signal ENCODING-UNKNOWN-ENCODING.
   USE-VALUE supplies another encoding name (normalized and checked again)."
  (loop
    (when (%babel-encoding-p encoding)
      (return encoding))
    (restart-case
        (error 'encoding-unknown-encoding
               :encoding encoding
               :message "no Babel character encoding by that name")
      (use-value (value)
        :report "Use another encoding"
        :interactive (lambda ()
                       (format *query-io* "Encoding: ")
                       (force-output *query-io*)
                       (list (read *query-io*)))
        (setf encoding (normalize-encoding value))))))

(defun %unencodable-index (string encoding)
  "Index of the first character of STRING that ENCODING cannot encode, or NIL."
  (loop for i from 0
        for ch across string
        unless (ignore-errors
                 (babel:string-to-octets (string ch) :encoding encoding)
                 t)
          return i))

(defun %babel-string-to-octets (string encoding)
  ;; Babel's own error for an unmappable character is unreliable: KOI8
  ;; encoders signal (ERROR T …), CP932 / EUC-JP a TYPE-ERROR on NIL.
  ;; Locate the character ourselves and report it.
  (handler-case
      (babel:string-to-octets string :encoding encoding)
    (error (e)
      (let ((index (%unencodable-index string encoding)))
        (error 'encoding-transcode-error
               :encoding encoding
               :message (if index
                            (let ((ch (char string index)))
                              (format nil "cannot encode '~C' (U+~4,'0X) at index ~D"
                                      ch (char-code ch) index))
                            (princ-to-string e)))))))

(defun %babel-octets-to-string (octets encoding)
  (handler-case
      (babel:octets-to-string octets :encoding encoding)
    (babel-encodings:character-coding-error (e)
      (error 'encoding-transcode-error
             :encoding encoding
             :message (princ-to-string e)))
    (error ()
      ;; Babel's multibyte decoders fail on truncated or unmapped input with
      ;; TYPE-ERROR / index errors rather than a coding error.
      (error 'encoding-transcode-error
             :encoding encoding
             :message (format nil "malformed ~A input (~D octets)"
                              encoding (length octets))))))

(defun %emit (out stream)
  (cond
    ((null stream) out)
    ((stringp out) (write-string out stream) out)
    (t (write-sequence out stream) out)))

(defun %encode-text (value encoding stream)
  (let ((encoding (%text-encoding encoding)))
    (unless (stringp value)
      (error 'encoding-encode-error
             :message (format nil "encode ~S wants a string, got ~S"
                              encoding (type-of value))))
    (%emit (%babel-string-to-octets value encoding) stream)))

(defun %encode-transfer (value encoding pad pad-p columns columns-p stream)
  (let* ((octets (%as-octets value))
         (out (ecase (%encoding-family encoding)
                (:rfc4648
                 (%wrap-columns
                  (%encode-octets octets (or (find-rfc4648-spec encoding)
                                             (error 'encoding-unknown-encoding
                                                    :encoding encoding))
                                  :pad (%effective-pad pad pad-p))
                  columns))
                (:quoted-printable
                 (%encode-quoted-printable octets
                                           :columns (if columns-p columns 76)))
                (:rle (%encode-rle octets)))))
    (%emit out stream)))

(defgeneric encode (value &key encoding pad columns stream)
  (:documentation
   "Encode VALUE.
    :text family (:utf-8 default, plus any Babel charset): string → octets.
    RFC 4648 / QP: octets (or string via UTF-8) → text. RLE → octets."))

(defmethod encode (value &key (encoding nil) (pad t pad-p)
                           (columns nil columns-p) stream)
  (let ((enc (%resolved-encoding encoding)))
    (if (eq (%encoding-family enc) :text)
        (%encode-text value enc stream)
        (%encode-transfer value enc pad pad-p columns columns-p stream))))

(defmethod encode ((value string) &key (encoding nil) (pad t pad-p)
                                    (columns nil columns-p) stream)
  (let ((enc (%resolved-encoding encoding)))
    (if (eq (%encoding-family enc) :text)
        (%encode-text value enc stream)
        (%encode-transfer value enc pad pad-p columns columns-p stream))))

(defun %decode-text (source encoding)
  (let ((encoding (%text-encoding encoding)))
    (etypecase source
      (string source)
      ((vector (unsigned-byte 8))
       (%babel-octets-to-string source encoding))
      (vector
       (%babel-octets-to-string (%as-octets source) encoding))
      (stream
       (%babel-octets-to-string
        (let ((out (make-array 0 :element-type '(unsigned-byte 8)
                                 :adjustable t :fill-pointer 0)))
          (loop for b = (read-byte source nil nil)
                while b do (vector-push-extend b out))
          out)
        encoding)))))

(defun %decode-transfer (source encoding pad pad-p strict strict-p)
  (ecase (%encoding-family encoding)
    (:rfc4648
     (%decode-string (%as-ascii-string source)
                     (or (find-rfc4648-spec encoding)
                         (error 'encoding-unknown-encoding :encoding encoding))
                     :pad (%effective-pad pad pad-p)
                     :strict (%effective-strict strict strict-p)))
    (:quoted-printable
     (%decode-quoted-printable (%as-ascii-string source)))
    (:rle
     (%decode-rle (%as-octets source)))))

(defgeneric decode (source &key encoding pad strict)
  (:documentation
   "Decode SOURCE.
    :text family (:utf-8 default): octets → string (string source is identity).
    :UTF-8 with :STRICT T uses DECODE-UTF-8-STRICT (no overlong forms or surrogates).
    RFC 4648 / QP / RLE: text → octets."))

(defmethod decode (source &key (encoding nil) (pad t pad-p) (strict nil strict-p))
  (let ((enc (%resolved-encoding encoding)))
    (if (eq (%encoding-family enc) :text)
        (if (and (eq enc :utf-8) (%effective-strict strict strict-p)
                 (vectorp source) (not (stringp source)))
            (decode-utf-8-strict source)
            (%decode-text source enc))
        (%decode-transfer source enc pad pad-p strict strict-p))))

(defun encode-to-octets (value &key encoding (pad t pad-p) columns)
  (let ((encoded (apply #'encode value :encoding encoding
                        (append (when pad-p (list :pad pad))
                                (when columns (list :columns columns))))))
    (if (and (vectorp encoded) (not (stringp encoded)))
        encoded
        (encode encoded))))

(defun decode-octets (octets &key encoding (pad t pad-p) (strict nil strict-p))
  (apply #'decode octets :encoding encoding
         (append (when pad-p (list :pad pad))
                 (when strict-p (list :strict strict)))))

(defun use-encoding-backend (&key (encoding :utf-8) (pad t) strict)
  (let ((backend (make-encoding-backend :encoding encoding :pad pad :strict strict)))
    (setf *encoding-backend* backend
          *encoding* (encoding-backend-encoding backend))
    backend))
