(in-package #:encoding-protocol/tests)

;;; On-demand charset systems (issue #12). Each case runs in a fresh SBCL
;;; image: this image loaded encoding-protocol/cjk already. The child loads
;;; the same .asd files this image used (no Quicklisp, no registry search),
;;; evaluates one form in ENCODING-PROTOCOL and prints the result after a
;;; marker line.

(defparameter *charset-systems*
  '("encoding-protocol/big5" "encoding-protocol/euc-kr"
    "encoding-protocol/gb18030" "encoding-protocol/iso-2022-jp"))

(defparameter *result-marker* "@@encoding-protocol-result@@ ")

(defun dependency-names (spec)
  "System names in a :DEPENDS-ON SPEC."
  (cond ((or (stringp spec) (symbolp spec)) (list (string-downcase (string spec))))
        ((and (consp spec) (eq (first spec) :version)) (dependency-names (second spec)))
        ((and (consp spec) (eq (first spec) :feature)) (dependency-names (third spec)))
        (t '())))

(defun asd-files (root)
  "The .asd files that define ROOT and every system it depends on, dependencies first."
  (let ((seen (make-hash-table :test 'equal))
        (files '()))
    (labels ((walk (name)
               (unless (or (gethash name seen)
                           (member name '("asdf" "uiop") :test #'string=)
                           (and (> (length name) 3) (string= "sb-" name :end2 3)))
                 (setf (gethash name seen) t)
                 (let ((system (asdf:find-system name)))
                   (dolist (spec (append (asdf:system-defsystem-depends-on system)
                                         (asdf:system-depends-on system)))
                     (mapc #'walk (dependency-names spec)))
                   (let ((file (asdf:system-source-file system)))
                     (when file
                       (pushnew (namestring file) files :test #'string=)))))))
      (walk root))
    (reverse files)))

(defun source-registry ()
  "A source registry of exactly the directories this image loaded
   encoding-protocol/cjk and its dependencies from: the child must not find
   other copies through the user's inherited configuration."
  `(:source-registry
    ,@(mapcar (lambda (file) (list :directory (directory-namestring file)))
              (remove-duplicates (asd-files "encoding-protocol/cjk")
                                 :test #'string= :key #'directory-namestring
                                 :from-end t))
    :ignore-inherited-configuration))

(defun child-program (form)
  "The program a fresh image loads: set up ASDF, load encoding-protocol, then
   evaluate FORM and print its result after *RESULT-MARKER*. ASCII only, so the
   child reads it the same way whatever its default external format."
  (format nil "(require \"asdf\")
(let ((*standard-output* (make-broadcast-stream))
      (*error-output* (make-broadcast-stream)))
  (asdf:initialize-source-registry '~A)
  (asdf:load-system \"encoding-protocol\"))
(in-package \"ENCODING-PROTOCOL\")
;; Loud defaults: an on-demand load must still keep stdout clean.
(setf *load-verbose* t *load-print* t *compile-verbose* t *compile-print* t)
(let ((result ~A))
  (with-standard-io-syntax
    (let ((*package* (find-package \"KEYWORD\")))
      (format t \"~~&~A~~S~~%\" result))))
"
          (with-standard-io-syntax (prin1-to-string (source-registry)))
          form *result-marker*))

(deftest zz-windows-diagnostic
  ;; Temporary: which way of starting the child works on Windows CI.
  #+sbcl
  (let* ((runtime (uiop:native-namestring sb-ext:*runtime-pathname*))
         (core (uiop:native-namestring sb-ext:*core-pathname*))
         (true-core (uiop:native-namestring (truename sb-ext:*core-pathname*)))
         (heap (format nil "~DMB" (floor (sb-ext:dynamic-space-size) (* 1024 1024))))
         (variants
           (list (list runtime "--version")
                 (list runtime "--core" core "--version")
                 (list runtime "--core" true-core "--version")
                 (list runtime "--core" true-core "--noinform" "--non-interactive"
                       "--eval" "(sb-ext:exit :code 3)")
                 (list runtime "--core" true-core "--noinform" "--dynamic-space-size" heap
                       "--non-interactive" "--no-sysinit" "--no-userinit"
                       "--eval" "(sb-ext:exit :code 3)"))))
    (dolist (v variants)
      (multiple-value-bind (out err code)
          (uiop:run-program v :output :string :error-output :string :ignore-error-status t)
        (format t "~&DIAG ~S => ~S ~S ~S~%" v code
                (subseq out 0 (min 300 (length out)))
                (subseq err 0 (min 300 (length err)))))))
  (ok t))

(defun run-in-fresh-image (form)
  "Evaluate FORM (a string, read in ENCODING-PROTOCOL) in a fresh SBCL; its
   printed result, or (:CHILD-FAILED exit-code stdout stderr) when it did not
   finish. The program goes through a file: Windows command lines mangle
   quotes and newlines in --eval arguments."
  #+sbcl
  (let ((file (uiop:tmpize-pathname
               (merge-pathnames "encoding-protocol-child.lisp" (uiop:temporary-directory)))))
    (unwind-protect
        (let ((command (list (uiop:native-namestring sb-ext:*runtime-pathname*)
                             "--core" (uiop:native-namestring sb-ext:*core-pathname*)
                             "--noinform" "--non-interactive" "--no-sysinit" "--no-userinit"
                             "--load" (uiop:native-namestring file))))
          (with-open-file (out file :direction :output :if-exists :supersede
                                    :external-format :utf-8)
            (write-string (child-program form) out))
          (multiple-value-bind (code stdout stderr)
              (process-protocol:run
               command
               :backend (process-backend-uiop:make-uiop-process-backend)
               :output :string :error-output :string :timeout 600)
            (let ((start (search *result-marker* stdout)))
              (flet ((tail (string) (subseq string (max 0 (- (length string) 2000)))))
                (if (and (eql code 0) start)
                    (with-standard-io-syntax
                        (let ((*read-eval* nil))
                          (read-from-string stdout t nil
                                            :start (+ start (length *result-marker*)))))
                    ;; Diagnostic (temporary): the same argv through UIOP directly.
                    (multiple-value-bind (uiop-out uiop-err uiop-code)
                        (uiop:run-program command :output :string :error-output :string
                                                  :ignore-error-status t)
                      (list :child-failed code (tail stdout) (tail stderr)
                            :command command
                            :uiop uiop-code (tail uiop-out) (tail uiop-err))))))))
      (when (probe-file file)
        (delete-file file))))
  #-sbcl
  (declare (ignore form)))

(deftest-parametrize fresh-image-charsets
    ((form expected)
     :ids ("big5-auto-loads-only-big5"
           "cp949-alias-loads-euc-kr-only"
           "auto-load-off-signals-with-system"
           "load-charset-system-restart"
           "load-failure-is-typed"
           "no-asdf-is-typed"
           "concurrent-first-use-loads-once"
           "load-charsets-eager"
           "umbrella-cjk-provides-all")
     ;; First use of "big5" loads encoding-protocol/big5 and nothing else,
     ;; quietly.
     ("(let* ((out (make-string-output-stream))
              (octets (let ((*standard-output* out))
                        (encode (map 'string #'code-char '(20013 25991)) :encoding \"big5\"))))
         (list (coerce octets 'list)
               (map 'list #'char-code (decode octets :encoding :big5))
               (mapcar #'asdf:component-loaded-p
                       '(\"encoding-protocol/big5\" \"encoding-protocol/euc-kr\"
                         \"encoding-protocol/gb18030\" \"encoding-protocol/iso-2022-jp\"))
               *charset-systems-loaded*
               (get-output-stream-string out)))"
      (quote ((164 164 164 229) (20013 25991) (t nil nil nil) ("encoding-protocol/big5") "")))
     ;; NORMALIZE-ENCODING loads too; cp949 lives in encoding-protocol/euc-kr.
     ("(list (multiple-value-list (normalize-encoding \"ks_c_5601-1987\"))
             (mapcar #'asdf:component-loaded-p
                     '(\"encoding-protocol/big5\" \"encoding-protocol/euc-kr\"
                       \"encoding-protocol/gb18030\" \"encoding-protocol/iso-2022-jp\")))"
      (quote ((:cp949 nil) (nil t nil nil))))
     ("(let ((*auto-load-charsets* nil))
         (list (handler-case (encode \"a\" :encoding \"big5\")
                 (encoding-unknown-encoding (c) (encoding-unknown-encoding-system c)))
               (multiple-value-list (normalize-encoding \"big5\"))
               (asdf:component-loaded-p \"encoding-protocol/big5\")))"
      (quote ("encoding-protocol/big5" (:big5 "encoding-protocol/big5") nil)))
     ("(let ((*auto-load-charsets* nil))
         (list (coerce (handler-bind ((encoding-unknown-encoding #'load-charset-system))
                         (encode (string (code-char 20013)) :encoding :big5))
                       'list)
               (asdf:component-loaded-p \"encoding-protocol/big5\")))"
      (quote ((164 164) t)))
     ;; A system ASDF cannot find: the typed condition, carrying ASDF's error.
     ("(let ((*optional-charsets* (acons :big5 \"encoding-protocol/no-such-charset\"
                                         *optional-charsets*)))
         (handler-case (encode \"a\" :encoding \"big5\")
           (encoding-unknown-encoding (c)
             (list (encoding-unknown-encoding-encoding c)
                   (encoding-unknown-encoding-system c)
                   (typep (encoding-unknown-encoding-load-error c) 'asdf:missing-component)))))"
      (quote (:big5 "encoding-protocol/no-such-charset" t)))
     ;; A delivered image without ASDF.
     ("(progn
         (rename-package \"ASDF\" \"ASDF-HIDDEN\")
         (handler-case (decode #(164 164) :encoding \"big5\")
           (encoding-unknown-encoding (c)
             (list (encoding-unknown-encoding-system c)
                   (typep (encoding-unknown-encoding-load-error c) 'error)))))"
      (quote ("encoding-protocol/big5" t)))
     ;; Four threads ask for big5 at once: one load, four results.
     ("(let* ((start nil)
              (text (map 'string #'code-char '(20013 25991)))
              (threads (loop repeat 4
                             collect (bt2:make-thread
                                      (lambda ()
                                        (loop until start do (bt2:thread-yield))
                                        (coerce (encode text :encoding \"big5\") 'list))))))
         (setf start t)
         (list (mapcar #'bt2:join-thread threads) *charset-systems-loaded*))"
      (quote
       (((164 164 164 229) (164 164 164 229) (164 164 164 229) (164 164 164 229))
        ("encoding-protocol/big5"))))
     ("(list (load-charsets \"big5\" :cp949 :utf-8)
             (mapcar #'asdf:component-loaded-p
                     '(\"encoding-protocol/big5\" \"encoding-protocol/euc-kr\"
                       \"encoding-protocol/gb18030\" \"encoding-protocol/iso-2022-jp\"))
             (load-charsets :all)
             (mapcar #'asdf:component-loaded-p
                     '(\"encoding-protocol/big5\" \"encoding-protocol/euc-kr\"
                       \"encoding-protocol/gb18030\" \"encoding-protocol/iso-2022-jp\"))
             (handler-case (load-charsets \"no-such-charset\")
               (encoding-unknown-encoding (c) (encoding-unknown-encoding-encoding c))))"
      (quote
       (("encoding-protocol/big5" "encoding-protocol/euc-kr")
        (t t nil nil)
        ("encoding-protocol/iso-2022-jp" "encoding-protocol/big5"
         "encoding-protocol/euc-kr" "encoding-protocol/gb18030")
        (t t t t)
        :no-such-charset)))
     ;; The umbrella system still provides every charset, with no on-demand load.
     ("(let ((*standard-output* (make-broadcast-stream)))
         (asdf:load-system \"encoding-protocol/cjk\")
         (list (mapcar #'asdf:component-loaded-p
                       '(\"encoding-protocol/big5\" \"encoding-protocol/euc-kr\"
                         \"encoding-protocol/gb18030\" \"encoding-protocol/iso-2022-jp\"))
               (let ((*auto-load-charsets* nil))
                 (mapcar (lambda (name) (nth-value 1 (normalize-encoding name)))
                         '(:iso-2022-jp :big5 :euc-kr :cp949 :gb18030)))
               *charset-systems-loaded*))"
      (quote ((t t t t) (nil nil nil nil nil) nil))))
  #-sbcl (skip "fresh-image tests spawn SBCL")
  #+sbcl (ok (equal (run-in-fresh-image form) expected)))

(deftest umbrella-cjk-in-this-image
  (dolist (system *charset-systems*)
    (ok (asdf:component-loaded-p system) system))
  (dolist (name '("iso-2022-jp" "big5" "euc-kr" "cp949" "gb18030"))
    (ok (null (nth-value 1 (normalize-encoding name))) name)))
