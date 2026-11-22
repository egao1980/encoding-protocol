(in-package #:encoding-protocol)

;;; RFC 3986 §2.1 percent-encoding over octets, plus strict UTF-8 text decode.
;;; Unreserved = ALPHA / DIGIT / "-" / "." / "_" / "~" (ASCII only).

(defun %unreserved-p (octet)
  (or (<= #x41 octet #x5A)              ; A-Z
      (<= #x61 octet #x7A)              ; a-z
      (<= #x30 octet #x39)              ; 0-9
      (member octet '(#x2D #x2E #x5F #x7E))))

(defun percent-encode (value &key (safe "") (space-as-plus nil))
  "RFC 3986 percent-encode VALUE (a string, encoded as UTF-8, or octets).
   Unreserved ASCII octets pass through; every other octet becomes %XX with
   upper-case hex. SAFE is a string of extra ASCII characters left as is
   (e.g. \"/\" for a path). SPACE-AS-PLUS writes a space as + (form style)."
  (let ((octets (%as-octets value))
        (safe-octets (map 'list (lambda (c)
                                  (let ((code (char-code c)))
                                    (unless (< code #x80)
                                      (error 'encoding-encode-error
                                             :message (format nil "percent-encode :safe ~S is not ASCII" c)))
                                    code))
                          safe)))
    (with-output-to-string (out)
      (loop for octet across octets
            do (cond ((or (%unreserved-p octet) (member octet safe-octets))
                      (write-char (code-char octet) out))
                     ((and space-as-plus (= octet #x20))
                      (write-char #\+ out))
                     (t
                      (write-char #\% out)
                      (write-char (char "0123456789ABCDEF" (ash octet -4)) out)
                      (write-char (char "0123456789ABCDEF" (logand octet #xF)) out)))))))

(defun percent-decode-octets (string &key (plus-as-space nil))
  "RFC 3986 percent-decode STRING to octets. A % not followed by two hex
   digits, or a non-ASCII character, signals ENCODING-DECODE-ERROR.
   PLUS-AS-SPACE decodes + as a space (form style)."
  (let ((out (make-array (length string) :element-type '(unsigned-byte 8)
                                         :fill-pointer 0))
        (n (length string))
        (i 0))
    (loop while (< i n)
          do (let ((c (char string i)))
               (cond
                 ((char= c #\%)
                  (unless (and (<= (+ i 3) n)
                               (%hex-digit-p (char string (+ i 1)))
                               (%hex-digit-p (char string (+ i 2))))
                    (error 'encoding-decode-error
                           :character c
                           :message (format nil "malformed percent-escape at ~D in ~S" i string)))
                  (vector-push (%hex-byte (char string (+ i 1)) (char string (+ i 2))) out)
                  (incf i 3))
                 ((>= (char-code c) #x80)
                  (error 'encoding-decode-error
                         :character c
                         :message (format nil "non-ASCII ~S at ~D in percent-encoded text" c i)))
                 ((and plus-as-space (char= c #\+))
                  (vector-push #x20 out) (incf i))
                 (t (vector-push (char-code c) out) (incf i)))))
    (coerce out '(simple-array (unsigned-byte 8) (*)))))

(defun %utf-8-error (octets i what)
  (error 'encoding-decode-error
         :message (format nil "invalid UTF-8 at octet ~D: ~A (~{~2,'0X~^ ~})"
                          i what
                          (coerce (subseq octets i (min (length octets) (+ i 4))) 'list))))

(defun decode-utf-8-strict (octets &key (start 0) (end (length octets)))
  "Decode OCTETS as UTF-8 per RFC 3629 §3 / Unicode Table 3-7. Overlong forms
   (C0 AF, E0 80 AF, …), surrogates (ED A0 80), code points above U+10FFFF,
   stray continuation bytes and truncated sequences signal ENCODING-DECODE-ERROR."
  (let ((octets (%as-octets octets)))
    (with-output-to-string (out)
      (let ((i start))
        (loop while (< i end)
              do (let ((b0 (aref octets i)))
                   (multiple-value-bind (len lo hi init)
                       (cond ((< b0 #x80) (values 1 0 0 b0))
                             ((<= #xC2 b0 #xDF) (values 2 #x80 #xBF (logand b0 #x1F)))
                             ((= b0 #xE0) (values 3 #xA0 #xBF (logand b0 #x0F)))
                             ((= b0 #xED) (values 3 #x80 #x9F (logand b0 #x0F)))
                             ((<= #xE1 b0 #xEF) (values 3 #x80 #xBF (logand b0 #x0F)))
                             ((= b0 #xF0) (values 4 #x90 #xBF (logand b0 #x07)))
                             ((<= #xF1 b0 #xF3) (values 4 #x80 #xBF (logand b0 #x07)))
                             ((= b0 #xF4) (values 4 #x80 #x8F (logand b0 #x07)))
                             (t (%utf-8-error octets i
                                              (if (<= #x80 b0 #xBF)
                                                  "unexpected continuation byte"
                                                  "invalid lead byte (overlong or out of range)"))))
                     (when (> (+ i len) end)
                       (%utf-8-error octets i "truncated sequence"))
                     (let ((code init))
                       (loop for k from 1 below len
                             for b = (aref octets (+ i k))
                             do (unless (if (= k 1) (<= lo b hi) (<= #x80 b #xBF))
                                  (%utf-8-error octets i
                                                (if (= k 1)
                                                    "overlong, surrogate or out-of-range sequence"
                                                    "bad continuation byte")))
                                (setf code (logior (ash code 6) (logand b #x3F))))
                       (write-char (code-char code) out))
                     (incf i len))))))))

(defun percent-decode (string &key (plus-as-space nil))
  "Percent-decode STRING and decode the octets as strict UTF-8.
   Malformed escapes or invalid UTF-8 (overlong, surrogates, …) signal
   ENCODING-DECODE-ERROR."
  (decode-utf-8-strict (percent-decode-octets string :plus-as-space plus-as-space)))
