(in-package #:encoding-protocol)

;;; On-demand charset systems (issue #12). A charset in *OPTIONAL-CHARSETS*
;;; lives in its own ASDF system. ENCODE / DECODE / NORMALIZE-ENCODING load it
;;; on first use (*AUTO-LOAD-CHARSETS*), before any Babel call, and
;;; LOAD-CHARSETS loads charsets eagerly (delivered images).
;;;
;;; One image-wide lock serializes the loads, so concurrent first uses load a
;;; system once. While it loads, Babel's global tables are bound to private
;;; copies in the loading thread and published when the load succeeds: other
;;; threads keep reading complete tables, and a failed load leaves Babel as it
;;; was.

(defvar *charset-load-lock* (bt2:make-recursive-lock :name "encoding-protocol charset loading")
  "Held while a charset system loads: one load at a time in the image.")

(defvar *charset-systems-loaded* '()
  "Charset systems this image loaded on demand or through LOAD-CHARSETS, in
   load order. A system its dependents loaded through ASDF is not listed.")

(defvar *charset-systems-loading* '()
  "Charset systems the current thread is loading.")

(defparameter *babel-tables*
  '(("BABEL-ENCODINGS" . "*SUPPORTED-CHARACTER-ENCODINGS*")
    ("BABEL-ENCODINGS" . "*CHARACTER-ENCODINGS*")
    ("BABEL-ENCODINGS" . "*ABSTRACT-MAPPINGS*")
    ("BABEL" . "*STRING-VECTOR-MAPPINGS*")
    ("BABEL" . "*SIMPLE-BASE-STRING-VECTOR-MAPPINGS*"))
  "Babel's global tables a charset system adds to (package . variable). Babel
   reads each through its variable on every call.")

(defun %babel-table-symbols ()
  (loop for (package . name) in *babel-tables*
        for symbol = (and (find-package package) (find-symbol name package))
        when (and symbol (boundp symbol))
          collect symbol))

(defun %private-copy (value)
  (typecase value
    (hash-table (alexandria:copy-hash-table value))
    (list (copy-list value))
    (t value)))

(defun %read-encoding ()
  (format *query-io* "Encoding: ")
  (force-output *query-io*)
  (list (read *query-io*)))

(defun %unknown-encoding (encoding system failure)
  "The ENCODING-UNKNOWN-ENCODING to signal for ENCODING: SYSTEM defines it,
   FAILURE is the condition from loading SYSTEM."
  (make-condition 'encoding-unknown-encoding
                  :encoding encoding
                  :system system
                  :load-error failure
                  :message (cond (failure "its ASDF system did not load")
                                 (system "defined by a system that is not loaded")
                                 (t "no Babel character encoding by that name"))))

(defun %asdf-function (name)
  "ASDF's function NAME, or NIL in an image without ASDF."
  (let* ((package (find-package "ASDF"))
         (symbol (and package (find-symbol name package))))
    (and symbol (fboundp symbol) (symbol-function symbol))))

(defun %asdf-load-quietly (system)
  "Load SYSTEM with ASDF, writing nothing to *STANDARD-OUTPUT*. NIL, or the
   condition that stopped the load."
  (let ((load-system (%asdf-function "LOAD-SYSTEM")))
    (if (null load-system)
        (make-condition 'simple-error
                        :format-control "ASDF is not loaded in this image, so ~A cannot be loaded"
                        :format-arguments (list system))
        (handler-case
            (let ((*standard-output* (make-broadcast-stream))
                  (*compile-verbose* nil)
                  (*compile-print* nil)
                  (*load-verbose* nil)
                  (*load-print* nil))
              (funcall load-system system)
              nil)
          ;; The caller signals ENCODING-UNKNOWN-ENCODING with this as its
          ;; LOAD-ERROR: a missing system or a failed compile is never a raw
          ;; ASDF error to callers of ENCODE / DECODE.
          (error (condition) condition)))))

(defun %load-charset-system (system)
  "Load charset SYSTEM unless this image already did. NIL, or the condition
   from a load that failed."
  (bt2:with-recursive-lock-held (*charset-load-lock*)
    (cond
      ((member system *charset-systems-loaded* :test #'string=) nil)
      ((member system *charset-systems-loading* :test #'string=)
       (make-condition 'simple-error
                       :format-control "~A is already loading in this thread"
                       :format-arguments (list system)))
      (t
       (let* ((symbols (cons '*charset-systems-loaded* (%babel-table-symbols)))
              (tables (mapcar (lambda (symbol) (%private-copy (symbol-value symbol))) symbols))
              (failure nil))
         (progv symbols tables
           (let ((*charset-systems-loading* (cons system *charset-systems-loading*)))
             (setf failure (%asdf-load-quietly system)))
           (unless failure
             (setf *charset-systems-loaded* (append *charset-systems-loaded* (list system))
                   tables (mapcar #'symbol-value symbols))))
         (unless failure
           (mapc (lambda (symbol table) (setf (symbol-value symbol) table)) symbols tables))
         failure)))))

(defun %charset-load-failure (encoding &optional force)
  "Load the ASDF system that defines charset ENCODING when Babel lacks it and
   *AUTO-LOAD-CHARSETS* (or FORCE) is true. (values system failure): SYSTEM is
   the system ENCODING still needs (NIL when it needs none), FAILURE the
   condition from a load that did not give Babel ENCODING (NIL when no load
   was tried)."
  (let ((system (%missing-charset-system encoding)))
    (if (or (null system) (not (or force *auto-load-charsets*)))
        (values system nil)
        (let ((failure (%load-charset-system system)))
          (if (%missing-charset-system encoding)
              (values system
                      (or failure
                          (make-condition 'simple-error
                                          :format-control "~A loaded but did not define ~S"
                                          :format-arguments (list system encoding))))
              (values nil nil))))))

(defun load-charset-system (&optional condition)
  "Invoke the LOAD-CHARSET-SYSTEM restart of CONDITION (an
   ENCODING-UNKNOWN-ENCODING): load the ASDF system that defines its charset,
   then retry. Returns NIL when there is no such restart, as USE-VALUE does.
   Usable as a handler: (handler-bind ((encoding-unknown-encoding
   #'load-charset-system)) …)."
  (let ((restart (find-restart 'load-charset-system condition)))
    (when restart
      (invoke-restart restart))))

(defun load-charsets (&rest names)
  "Load now the ASDF systems that define the charsets NAMES (strings or
   symbols, as NORMALIZE-ENCODING takes them), or every charset in
   *OPTIONAL-CHARSETS* when NAMES has :ALL. Loads whatever
   *AUTO-LOAD-CHARSETS* says. A charset Babel already has needs nothing.
   For delivered images: call it before SAVE-LISP-AND-DIE, and ENCODE /
   DECODE need no ASDF at run time.
   An unknown name, or a failed load, signals ENCODING-UNKNOWN-ENCODING (see
   ENCODE). Returns the systems that define NAMES, in order."
  (let ((systems '()))
    (dolist (name (if (member :all names) (mapcar #'car *optional-charsets*) names))
      (let ((encoding (%text-encoding (%normalize-name name) t)))
        (let ((system (cdr (assoc encoding *optional-charsets*))))
          (when system
            (pushnew system systems :test #'string=)))))
    (nreverse systems)))
