(in-package #:encoding-protocol)

;;; Classic byte RLE. Not Parquet hybrid RLE/bit-packing (that stays in arrow-protocol).

(defun rle-runs (values)
  "Sequence → list of (count . value) runs, count ≥ 1."
  (let ((vals (coerce values 'vector))
        (runs '()))
    (when (plusp (length vals))
      (let ((v (aref vals 0))
            (n 1))
        (loop for i from 1 below (length vals)
              do (if (eql (aref vals i) v)
                     (incf n)
                     (progn
                       (push (cons n v) runs)
                       (setf v (aref vals i) n 1))))
        (push (cons n v) runs)))
    (nreverse runs)))

(defun expand-rle-runs (runs)
  "List of (count . value) → vector."
  (let* ((n (loop for r in runs sum (car r)))
         (out (make-array n))
         (i 0))
    (dolist (r runs)
      (loop repeat (car r)
            do (setf (aref out i) (cdr r))
               (incf i)))
    out))

(defun %encode-rle (octets)
  (let ((out (make-array (max 2 (* 2 (length octets)))
                         :element-type '(unsigned-byte 8)
                         :adjustable t
                         :fill-pointer 0)))
    (dolist (run (rle-runs octets))
      (let ((n (car run))
            (v (cdr run)))
        (unless (typep v '(unsigned-byte 8))
          (error 'encoding-encode-error
                 :message (format nil "RLE byte value out of range: ~S" v)))
        (loop
          (let ((chunk (min n 255)))
            (vector-push-extend chunk out)
            (vector-push-extend v out)
            (decf n chunk)
            (when (zerop n) (return))))))
    (let ((bytes (make-array (length out) :element-type '(unsigned-byte 8))))
      (replace bytes out)
      bytes)))

(defun %decode-rle (octets)
  (let ((n (length octets)))
    (unless (evenp n)
      (error 'encoding-decode-error :message "RLE payload length must be even"))
    (let ((out (make-array 16 :element-type '(unsigned-byte 8)
                           :adjustable t :fill-pointer 0)))
      (loop for i from 0 below n by 2
            for count = (aref octets i)
            for value = (aref octets (1+ i))
            do (loop repeat count do (vector-push-extend value out)))
      (let ((bytes (make-array (length out) :element-type '(unsigned-byte 8))))
        (replace bytes out)
        bytes))))
