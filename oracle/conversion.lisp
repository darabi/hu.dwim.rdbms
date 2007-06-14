(in-package :cl-rdbms.oracle)

(enable-sharp-boolean-syntax)

;;;
;;; Boolean conversions
;;;

(defun boolean-to-char (value)
  (values
   (cffi::foreign-alloc :char :initial-element (if value #\T #\F))
   1))

(defun boolean-from-char (ptr len)
  (assert (= len 1))
  (ecase (cffi:mem-aref ptr :char)
             (#\T #t)
             (#\F #f)))

;;;
;;; Integer conversions
;;;

(defun integer-to-int8 (value)
  (assert (typep value '(signed-byte 8)))
  (values
   (cffi:foreign-alloc 'oci:sb-1 :initial-element value)
   1))

(defun integer-from-int8 (ptr len)
  (assert (= len 1))
  (cffi:mem-ref ptr 'oci:sb-1))

(defun integer-to-int16 (value)
  (assert (typep value '(signed-byte 16)))
  (values
   (cffi:foreign-alloc 'oci:sb-2 :initial-element value)
   2))

(defun integer-from-int16 (ptr len)
  (assert (= len 2))
  (cffi:mem-ref ptr 'oci:sb-2))

(defun integer-to-int32 (value)
  (assert (typep value '(signed-byte 32)))
  (values
   (cffi:foreign-alloc 'oci:sb-4 :initial-element value)
   4))

(defun integer-from-int32 (ptr len)
  (assert (= len 4))
  (cffi:mem-ref ptr 'oci:sb-4))

(defun integer-to-varnum (value)
  (assert (typep value 'integer))
  (rational-to-varnum value))

(defun integer-from-varnum (ptr len)
  (declare (ignore ptr len))
  nil) ; TODO

;;;
;;; Float conversions
;;;

(defun float-to-bfloat (value)
  (values
   (cffi:foreign-alloc :float :initial-element (coerce value 'single-float))
   4))

(defun float-from-bfloat (ptr len)
  (assert (= len 4))
  (cffi:mem-ref ptr :float))

(defun double-to-bdouble (value)
  (values
   (cffi:foreign-alloc :double :initial-element (coerce value 'double-float))
   8))

(defun double-from-bdouble (ptr len)
  (assert (= len 8))
  (cffi:mem-ref ptr :double))

;;;
;;; Numeric conversions
;;;

(defun rational-to-number (rational &key (precision 38) (scale 0))
  (let ((bytes (rational-to-byte-array rational precision scale)))
    (cffi:foreign-alloc 'oci:ub-1 :count (length bytes) :initial-contents bytes)))

(defun rational-from-number (ptr len)
  (declare (ignore ptr len))
  nil) ; TODO

(defun rational-to-varnum (rational &key (precision 38) (scale 0))
  (let* ((bytes (rational-to-byte-array rational precision scale))
         (len (length bytes))
         (varnum (cffi:foreign-alloc 'oci:ub-1 :count (1+ len))))
    (setf (cffi:mem-aref varnum 'oci:ub-1 0) len)
    (loop for i from 0 below len
          do (setf (cffi:mem-aref varnum 'oci:ub-1 (1+ i)) (aref bytes i)))
    varnum))

(defun rational-from-varnum (ptr len)
  (declare (ignore ptr len))
  nil) ; TODO

;;;
;;; Character data conversions
;;;

(defun string-to-string (value)
  (values
   (cffi:foreign-string-alloc value)
   (1+ (length value))))

(defun string-from-string (ptr len)
  (declare (ignore len))
  (cffi:foreign-string-to-lisp ptr))

(defun string-to-long-varchar (str)
  (let* ((len (length str))
         (ptr (cffi::foreign-alloc :char :count (+ len 5)))) ; +4 for length,
                                                             ; +1 for ending NULL character added
                                                             ; by cffi
    (setf (cffi:mem-ref ptr :int32) len)
    (cffi:lisp-string-to-foreign str (cffi:inc-pointer ptr 4) len)
    (values ptr len)))

(defun string-from-long-varchar (ptr len)
  (declare (ignore ptr len))
  nil) ; TODO

;;;
;;; Binary data conversions
;;;
(defun byte-array-to-long-varraw (ba)
  (assert (typep ba '(vector unsigned-byte)))
  (let* ((len (length ba))
         (ptr (cffi::foreign-alloc 'oci:ub-1 :count (+ len 4))))
    (setf (cffi:mem-ref ptr :int32) len)
    (loop for byte across ba
          for i from 4
          do (setf (cffi:mem-aref ptr 'oci:ub-1 i) byte))
    (values ptr len)))

(defun byte-array-from-long-varraw (ptr len)
  (declare (ignore ptr len))
  nil) ; TODO


;;;
;;; Datetime conversions
;;;

(defun local-time-to-date (local-time)
  (let ((date (cffi:foreign-alloc 'oci:ub-1 :count 7)))
    (multiple-value-bind (ms ss mm hh day month year dow dls tz) (decode-local-time local-time)
      (declare (ignore ms dow dls tz))
      (multiple-value-bind (century year) (floor year 100)
        (setf (cffi:mem-ref date 'oci:ub-1 0) (+ 100 century)
              (cffi:mem-ref date 'oci:ub-1 1) (+ 100 year)
              (cffi:mem-ref date 'oci:ub-1 2) month
              (cffi:mem-ref date 'oci:ub-1 3) day
              (cffi:mem-ref date 'oci:ub-1 4) (1+ hh)
              (cffi:mem-ref date 'oci:ub-1 5) (1+ mm)
              (cffi:mem-ref date 'oci:ub-1 6) (1+ ss))))
    (values date 7)))

(defun local-time-from-date (ptr len)
  (declare (ignore ptr len))
  nil) ; TODO

(defun local-time-to-oci-date (local-time)
  (let ((oci-date (cffi:foreign-alloc 'oci:date)))
    (multiple-value-bind (yyyy mm dd) (local-time::local-time-decoded-date local-time)
      (setf (cffi:foreign-slot-value oci-date 'oci:date 'oci::date-yyyy) yyyy
            (cffi:foreign-slot-value oci-date 'oci:date 'oci::date-mm) mm
            (cffi:foreign-slot-value oci-date 'oci:date 'oci::date-dd) dd))
    oci-date))

(defun local-time-from-oci-date (ptr len)
  (declare (ignore ptr len))
  nil) ; TODO

(defun local-time-to-timestamp (local-time environment-handle error-handle &key with-timezone)
  (let ((oci-date-time-pointer (cffi::foreign-alloc :pointer))
        (timezone-str (timezone-as-HHMM-string local-time)))
    (if with-timezone
        (oci-call (oci:handle-alloc environment-handle
                                    oci-date-time-pointer
                                    oci:+dtype-timestamp-tz+
                                    0
                                    (cffi:null-pointer)))
        (oci-call (oci:handle-alloc environment-handle
                                    oci-date-time-pointer
                                    oci:+dtype-timestamp+
                                    0
                                    (cffi:null-pointer))))
    (multiple-value-bind (ms ss mm hh day month year) (decode-local-time local-time)
      (oci-call (oci:date-time-construct environment-handle
                                         error-handle
                                         (cffi:mem-ref oci-date-time-pointer :pointer)
                                         year
                                         month
                                         day
                                         hh
                                         mm
                                         ss
                                         (* 1000 ms) ; TODO check this
                                         (cffi:foreign-string-alloc timezone-str) ;FIXME freeing?
                                         (1+ (length timezone-str)))))
    oci-date-time-pointer))

(defun local-time-from-timestamp (ptr len)
  (declare (ignore ptr len))
  nil) ; TODO

;;;
;;; Helpers
;;;

;;
;; 1234567  -> 1 23 45 67  , e = 3
;; 1.234567 -> 1 23 45 67  , e = 0
;; 12.34567 -> 12 34 56 70 , e = 0

(defun rational-to-byte-array (rational &key (precision 38) (scale 0))
  "Returns the bytes of RATIONAL encoded as an Oracle NUMBER."
  (declare (ignore precision))
  (cond ((zerop rational) #(128))
        (t (let* ((number (if (evenp scale)
                              (round (* (abs rational) (expt 10 scale)))
                              (round (* (abs rational) (expt 10 (1+ scale))))))
                  (scale-100 (if (evenp scale)
                                 (/ scale 2)
                                 (/ (1+ scale) 2)))
                  (result (make-array 21 :adjustable #t :fill-pointer 0)))
             ;; place holder
             (vector-push-extend 0 result)
             ;; mantissa
             (loop for d in (base-100-digits number)
                   do (vector-push-extend
                       (if (> rational 0)
                           (1+ d)
                           (- 101 d))
                       result))
             ;; exponent
             (let ((base-100-exponent (- (length result) 2 scale-100)))
               (setf (aref result 0) (if (< rational 0)
                                         (- 62 base-100-exponent)
                                         (+ 193 base-100-exponent))))
             ;; end mark
             (when (and (< rational 0) (< (length result) 21))
               (vector-push-extend 102 result))

             result))))

(defun base-100-digits (number)
  "Returns the base-100 digits of NUMBER (a positive integer) as a list, the most significant
digit is the first."
  (assert (and (typep number 'integer) (>= number 0)))
  (let ((digits nil))
    (loop while (/= number 0)
      do (multiple-value-bind (n d) (floor number 100)
           (setf number n)
           (push d digits)))
    digits))

(defun timezone-as-HHMM-string (local-time)
  "Returns the time-zone of LOCAL-TIME in [+-]HH:MM format."
  (let ((offset (timezone local-time))) 
    (multiple-value-bind (hour min) (floor (abs offset) 3600)
      (format nil "~C~2,'0D:~2,'0D"
              (if (> offset 0) #\+ #\-)
              hour
              min))))

