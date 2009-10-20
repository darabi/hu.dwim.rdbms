;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.rdbms.oracle)

;;;;
;;;; Routines for converting the external representation of data types
;;;; from and to lisp values.
;;;; The naming of the functions follows the following scheme:
;;;;
;;;;   <lisp-type-name>-to-<external-type-name> and
;;;;   <lisp-type-name>-from-<external-type-name>
;;;;
;;;; where the external type names and their format is defined by
;;;;   <http://download-uk.oracle.com/docs/cd/B14117_01/appdev.101/b10779/oci03typ.htm#443569>

;;;;;;
;;; Boolean conversions

(def function boolean-to-char (value)
  (foreign-oci-string-alloc (if value "T" "F") :null-terminated-p #f))

(def function boolean-from-char (ptr len)
  (assert (= len (oci-char-width)))
  (let ((str (oci-string-to-lisp ptr len)))
    (assert (= (length str) 1))
    (let ((ch (char str 0)))
      (case ch
        (#\T #t)
        (#\F #f)
        (t ch)))))                ; KLUDGE real char(1), not a boolean

;;;;;;
;;; Integer conversions

(def function integer-to-int8 (value)
  (assert (typep value '(signed-byte 8)))
  (values
   (cffi:foreign-alloc 'oci:sb-1 :initial-element value)
   1))

(def function integer-from-int8 (ptr len)
  (assert (= len 1))
  (cffi:mem-ref ptr 'oci:sb-1))

(def function integer-to-int16 (value)
  (assert (typep value '(signed-byte 16)))
  (values
   (cffi:foreign-alloc 'oci:sb-2 :initial-element value)
   2))

(def function integer-from-int16 (ptr len)
  (assert (= len 2))
  (cffi:mem-ref ptr 'oci:sb-2))

(def function integer-to-int32 (value)
  (assert (typep value '(signed-byte 32)))
  (values
   (cffi:foreign-alloc 'oci:sb-4 :initial-element value)
   4))

(def function integer-from-int32 (ptr len)
  (assert (= len 4))
  (cffi:mem-ref ptr 'oci:sb-4))

(def function integer-to-varnum (value)
  (assert (typep value 'integer))
  (rational-to-varnum value))

(def function integer-from-varnum (ptr len)
  (let ((r (rational-from-varnum ptr len)))
    (assert (typep r 'integer))
    r))

;;;;;;
;;; Float conversions

(def function float-to-bfloat (value)
  (values
   (cffi:foreign-alloc :float :initial-element (coerce value 'single-float))
   4))

(def function float-from-bfloat (ptr len)
  (assert (= len 4))
  (cffi:mem-ref ptr :float))

(def function double-to-bdouble (value)
  (values
   (cffi:foreign-alloc :double :initial-element (coerce value 'double-float))
   8))

(def function double-from-bdouble (ptr len)
  (assert (= len 8))
  (cffi:mem-ref ptr :double))

;;;;;;
;;; Numeric conversions

(def function rational-to-number (rational &key (precision 38) (scale 0))
  (let ((bytes (rational-to-byte-array rational precision scale)))
    (cffi:foreign-alloc 'oci:ub-1 :count (length bytes) :initial-contents bytes)))

(def function rational-from-number (ptr len)
  (assert (<= 1 len 21))
  (let* ((sign-and-exponent (cffi:mem-aref ptr :uint8 0))
         (positivep (>= sign-and-exponent 128))
         (base-100-exponent (if positivep
                                (- sign-and-exponent 192)
                                (- 63 sign-and-exponent)))
         (mantissa 0))
    (if positivep
        (loop for i from 1 below len
              for base-100-digit = (- (cffi:mem-aref ptr :uint8 i) 1)
              do (decf base-100-exponent)
              do (setf mantissa (+ (* 100 mantissa) base-100-digit)))
        (loop for i from 1 below len
              for base-100-digit = (- 101 (cffi:mem-aref ptr :uint8 i))
              until (< base-100-digit 0) ; ending 102
              do (decf base-100-exponent)
              do (setf mantissa (+ (* 100 mantissa) base-100-digit))))
    (cond ((zerop mantissa) 0)
          ((zerop base-100-exponent) (if positivep mantissa (- mantissa)))
          (t (if positivep
                 (* mantissa (expt 100 base-100-exponent))
                 (- (* mantissa (expt 100 base-100-exponent))))))))

(def function rational-to-varnum (rational &key (precision 38) (scale 0))
  (let* ((bytes (rational-to-byte-array rational precision scale))
         (len (length bytes))
         (varnum (cffi:foreign-alloc 'oci:ub-1 :count (1+ len))))
    (setf (cffi:mem-aref varnum 'oci:ub-1 0) len)
    (loop for i from 0 below len
          do (setf (cffi:mem-aref varnum 'oci:ub-1 (1+ i)) (aref bytes i)))
    (values varnum (1+ len))))

(def function rational-from-varnum (ptr len)
  (assert (= len 22))
  (rational-from-number
   (cffi:inc-pointer ptr 1)
   (cffi:mem-aref ptr :uint8 0)))

;;;;;;
;;; Character data conversions

(def function string-to-string (value)
  (foreign-oci-string-alloc value))

(def function string-from-string (ptr length)
  (declare (ignore length)) ; null terminated
  (oci-string-to-lisp ptr))

(def function string-to-long-varchar (str)
  (let* ((encoding (connection-encoding-of (database-of *transaction*)))
         (character-width (cffi::null-terminator-len encoding))
         (str-len (* character-width (length str)))
         (ptr (cffi::foreign-alloc :char
                                   :count (+ (cffi:foreign-type-size 'oci:sb-4) ; length field
                                             str-len
                                             character-width ; for terminating null added by the cffi call
                                             ))))
    (setf (cffi:mem-ref ptr 'oci:sb-4) str-len)
    (cffi:lisp-string-to-foreign str
                                 (cffi:inc-pointer ptr (cffi:foreign-type-size 'oci:sb-4))
                                 (+ str-len character-width)
                                 :encoding encoding)
    (values ptr (+ str-len 4))))

(def function string-from-long-varchar (ptr len)
  (assert (>= len 4))
  (oci-string-to-lisp
   (cffi:inc-pointer ptr (cffi:foreign-type-size 'oci:sb-4))
   (cffi:mem-ref ptr 'oci:sb-4)))

;;;;;;
;;; Binary data conversions

(def function byte-array-to-long-varraw (ba)
  (assert (typep ba 'vector)) ; '(vector (unsigned-byte 8))
  (let* ((len (length ba))
         (ptr (cffi::foreign-alloc 'oci:ub-1 :count (+ len 4))))
    (setf (cffi:mem-ref ptr :int32) len)
    (loop for byte across ba
          for i from 4
          do (setf (cffi:mem-aref ptr 'oci:ub-1 i) byte))
    (values ptr (+ len 4))))

(def function byte-array-from-long-varraw (ptr len)
  (assert (>= len 4))
  (let* ((size (cffi:mem-ref ptr 'oci:sb-4))
         (result (make-array size)))
    (loop for i from 0 below size
          do (setf (aref result i)
                   (cffi:mem-aref ptr 'oci:ub-1 (+ 4 i))))
    result))


;;;;;;
;;; Datetime conversions

(def function local-time-to-date (local-time)
  (let ((date (cffi:foreign-alloc 'oci:ub-1 :count 7)))
    (multiple-value-bind (ms ss mm hh day month year dow dls tz) (decode-local-time local-time)
      (declare (ignore ms dow dls tz))
      (multiple-value-bind (century year) (floor year 100)
        (setf (cffi:mem-aref date 'oci:ub-1 0) (+ 100 century) ; TODO check BC dates
              (cffi:mem-aref date 'oci:ub-1 1) (+ 100 year)
              (cffi:mem-aref date 'oci:ub-1 2) month
              (cffi:mem-aref date 'oci:ub-1 3) day
              (cffi:mem-aref date 'oci:ub-1 4) (1+ hh)
              (cffi:mem-aref date 'oci:ub-1 5) (1+ mm)
              (cffi:mem-aref date 'oci:ub-1 6) (1+ ss))))
    (values date 7)))

(def function local-time-from-date (ptr len)
  (assert (= len 7))
  (let ((century (- (cffi:mem-aref ptr 'oci:ub-1 0) 100)) ; TODO BC dates
        (year (- (cffi:mem-aref ptr 'oci:ub-1 1) 100))
        (month (cffi:mem-aref ptr 'oci:ub-1 2))
        (day (cffi:mem-aref ptr 'oci:ub-1 3))
        (hour (1- (cffi:mem-aref ptr 'oci:ub-1 4)))
        (min (1- (cffi:mem-aref ptr 'oci:ub-1 5)))
        (sec (1- (cffi:mem-aref ptr 'oci:ub-1 6))))
    (encode-local-time 0
                       sec
                       min
                       hour
                       day
                       month
                       (+ (* 100 century) year)
                       :timezone +utc-zone+)))

(def function local-time-to-oci-date (local-time)
  ;; FIXME using fields of the opaque OCIDate structure, because the OCIDateSetDate and
  ;;       OCIDateSetTime macros are not available
  (multiple-value-bind (ms ss mm hh day month year) (decode-local-time local-time)
    (declare (ignore ms))
    (let* ((oci-date (cffi:foreign-alloc 'oci:date))
           (oci-time (cffi:foreign-slot-pointer oci-date 'oci:date 'oci::date-time)))
      (setf (cffi:foreign-slot-value oci-date 'oci:date 'oci::date-yyyy) year
            (cffi:foreign-slot-value oci-date 'oci:date 'oci::date-mm) month
            (cffi:foreign-slot-value oci-date 'oci:date 'oci::date-dd) day
            (cffi:foreign-slot-value oci-time 'oci:time 'oci::time-hh) hh
            (cffi:foreign-slot-value oci-time 'oci:time 'oci::time-hh) mm
            (cffi:foreign-slot-value oci-time 'oci:time 'oci::time-hh) ss)
      (values
       oci-date
       (cffi:foreign-type-size 'oci:date)))))

(def function local-time-from-oci-date (ptr len)
  ;; FIXME using fields of the opaque OCIDate structure, because the OCIDateGetDate and
  ;;       OCIDateGetTime macros are not available
  (assert (= len (cffi:foreign-type-size 'oci:date)))
  (let* ((year (cffi:foreign-slot-value ptr 'oci:date 'oci::date-yyyy))
         (month (cffi:foreign-slot-value ptr 'oci:date 'oci::date-mm))
         (day (cffi:foreign-slot-value ptr 'oci:date 'oci::date-dd))
         (oci-time (cffi:foreign-slot-value ptr 'oci:date 'oci::date-time))
         (hour (cffi:foreign-slot-value oci-time 'oci:time 'oci::time-hh))
         (min (cffi:foreign-slot-value oci-time 'oci:time 'oci::time-mi))
         (sec (cffi:foreign-slot-value oci-time 'oci:time 'oci::time-ss)))
    (encode-local-time 0
                       sec
                       min
                       hour
                       day
                       month
                       year
                       :timezone +utc-zone+)))

(def function local-time-to-timestamp (local-time)
  (let ((oci-date-time-pointer (cffi::foreign-alloc :pointer)))
    (oci-call (oci:descriptor-alloc (environment-handle-of *transaction*)
                                    oci-date-time-pointer
                                    oci:+dtype-timestamp+
                                    0
                                    null))
    (multiple-value-bind (ms ss mm hh day month year) (decode-local-time local-time)
      (oci-call (oci:date-time-construct (environment-handle-of *transaction*)
                                         (error-handle-of *transaction*)
                                         (cffi:mem-ref oci-date-time-pointer :pointer)
                                         year
                                         month
                                         day
                                         hh
                                         mm
                                         ss
                                         (* 1000 ms) ; TODO check this
                                         (cffi:null-pointer)
                                         0)))
    (values
     oci-date-time-pointer
     (cffi:foreign-type-size :pointer))))

(def function local-time-from-timestamp (ptr len)
  (assert (= len (cffi:foreign-type-size :pointer)))
  (let ((environment-handle (environment-handle-of *transaction*))
        (error-handle (error-handle-of *transaction*)))
    (cffi:with-foreign-objects ((year 'oci:sb-2)
                               (month 'oci:ub-1)
                               (day 'oci:ub-1)
                               (hour 'oci:ub-1)
                               (min 'oci:ub-1)
                               (sec 'oci:ub-1)
                               (fsec 'oci:ub-4))
     (let* ((oci-date-time-pointer (cffi:mem-ref ptr '(:pointer oci:date-time)))
            (oci-date-time (cffi:mem-ref oci-date-time-pointer 'oci:date-time)))
       (oci-call (oci:date-time-get-date environment-handle
                                         error-handle
                                         oci-date-time
                                         year
                                         month
                                         day))
       (oci-call (oci:date-time-get-time environment-handle
                                         error-handle
                                         oci-date-time
                                         hour
                                         min
                                         sec
                                         fsec))
       (encode-local-time (round (/ (cffi:mem-ref fsec 'oci:ub-4) 1000))
                          (cffi:mem-ref sec 'oci:ub-1)
                          (cffi:mem-ref min 'oci:ub-1)
                          (cffi:mem-ref hour 'oci:ub-1)
                          (cffi:mem-ref day 'oci:ub-1)
                          (cffi:mem-ref month 'oci:ub-1)
                          (cffi:mem-ref year 'oci:sb-2)
                          :timezone +utc-zone+)))))

(def function local-time-to-timestamp-tz (local-time)
  (let ((environment-handle (environment-handle-of *transaction*))
        (error-handle (error-handle-of *transaction*))
        (oci-date-time-pointer (cffi::foreign-alloc :pointer))
        (timezone-str (timezone-as-HHMM-string local-time)))
    (oci-call (oci:descriptor-alloc environment-handle
                                    oci-date-time-pointer
                                    oci:+dtype-timestamp-tz+
                                    0
                                    null))
    (multiple-value-bind (ms ss mm hh day month year) (decode-local-time local-time)
      (with-foreign-oci-string (timezone-str c-timezone-ptr c-timezone-size)
       (oci-call (oci:date-time-construct environment-handle
                                          error-handle
                                          (cffi:mem-ref oci-date-time-pointer :pointer)
                                          year
                                          month
                                          day
                                          hh
                                          mm
                                          ss
                                          (* 1000 ms)
                                          c-timezone-ptr
                                          c-timezone-size))))
    (values
     oci-date-time-pointer
     (cffi:foreign-type-size :pointer))))

(def function local-time-from-timestamp-tz (ptr len)
  (declare (ignore len))
  (let ((environment-handle (environment-handle-of *transaction*))
        (error-handle (error-handle-of *transaction*)))
    (cffi:with-foreign-objects ((year 'oci:sb-2)
                                (month 'oci:ub-1)
                                (day 'oci:ub-1)
                                (hour 'oci:ub-1)
                                (min 'oci:ub-1)
                                (sec 'oci:ub-1)
                                (fsec 'oci:ub-4)
                                (offset-hour 'oci:sb-1)
                                (offset-minute 'oci:sb-1))
      (let* ((oci-date-time-pointer (cffi:mem-ref ptr :pointer))
             (oci-date-time (cffi:mem-ref oci-date-time-pointer 'oci:date-time)))
        (oci-call (oci:date-time-get-date environment-handle
                                          error-handle
                                          oci-date-time
                                          year
                                          month
                                          day))
        (oci-call (oci:date-time-get-time environment-handle
                                          error-handle
                                          oci-date-time
                                          hour
                                          min
                                          sec
                                          fsec))
        (oci-call (oci:date-time-get-time-zone-offset environment-handle
                                                      error-handle
                                                      oci-date-time
                                                      offset-hour
                                                      offset-minute))

        (encode-local-time (round (/ (cffi:mem-ref fsec 'oci:ub-4) 1000))
                           (cffi:mem-ref sec 'oci:ub-1)
                           (cffi:mem-ref min 'oci:ub-1)
                           (cffi:mem-ref hour 'oci:ub-1)
                           (cffi:mem-ref day 'oci:ub-1)
                           (cffi:mem-ref month 'oci:ub-1)
                           (cffi:mem-ref year 'oci:sb-2)
                           :timezone (make-timezone
                                      (cffi:mem-ref offset-hour 'oci:sb-1)
                                      (cffi:mem-ref offset-minute 'oci:sb-1)))))))

;;;
;;; Helpers
;;;

(def function rational-to-byte-array (rational &optional (precision 38) (scale 0))
  "Returns the bytes of RATIONAL encoded as an Oracle NUMBER."
  (assert (<= 1 precision 38))
  (assert (<= -84 scale 127))
  (cond ((zerop rational) #.(coerce #(128) '(vector (unsigned-byte 8))))
        ((= rational 1) #.(coerce #(193 2) '(vector (unsigned-byte 8))))
        (t (let* ((negativep (< rational 0))
                  (mantissa)
                  (base-100-exponent)
                  (max-len)
                  (result (make-array 21 :adjustable #t :fill-pointer 0)))
             (if (evenp scale)
                 (setf mantissa (round (* (abs rational) (expt 10 scale)))
                       base-100-exponent (- (/ scale 2))
                       max-len (if (evenp precision)
                                   (1+ (/ precision 2))
                                   (1+ (/ (1+ precision) 2))))
                 (setf mantissa (round (* (abs rational) (expt 10 (1+ scale))))
                       base-100-exponent (- (/ (1+ scale) 2))
                       max-len (if (evenp precision)
                                   (+ 2 (/ precision 2))
                                   (+ 2 (/ (1+ precision) 2)))))
             ;; place holder
             (vector-push-extend 0 result)
             ;; mantissa
             (multiple-value-bind (base-100-digits length) (base-100-digits mantissa)
               (incf base-100-exponent length)
               (if negativep
                   (loop for d in base-100-digits
                         while (< (length result) max-len)
                         do (vector-push-extend (- 101 d) result))
                   (loop for d in base-100-digits
                         while (< (length result) max-len)
                         do (vector-push-extend (1+ d) result))))
             ;; exponent
             (setf (aref result 0) (if (< rational 0)
                                       (- 63 base-100-exponent)
                                       (+ 192 base-100-exponent)))
             ;; end mark
             (when (and negativep (< (length result) 21))
               (vector-push-extend 102 result))

             result))))

(def function base-100-digits (number)
  "Returns the base-100 digits of NUMBER (a positive integer) as a list, the most significant
digit is the first or NIL for 0."
  (declare (integer number))
  (assert (>= number 0))
  (let ((digits nil))
    (loop for count from 0
          while (/= number 0)
          do (multiple-value-bind (n d) (floor number 100)
               (setf number n)
               (push d digits))
          finally (return (values digits count)))))

(def function timezone-as-HHMM-string (local-time)
  "Returns the time-zone of LOCAL-TIME in [+-]HH:MM format."
  (let ((offset (timezone local-time))) 
    (multiple-value-bind (hour sec) (floor (abs offset) 3600)
      (format nil "~C~2,'0D:~2,'0D"
              (if (> offset 0) #\+ #\-)
              hour
              (floor sec 60)))))

(def function timezone-from-HHMM-string (timezone-string)
  "Parses the timezone from [+-]HH:MM format."
  (assert (= (length timezone-string) 6))
  (let ((sign (ecase (char timezone-string 0) (#\- -1) (#\+ 1)))
        (hours (parse-integer timezone-string :start 1 :end 3))
        (minutes (parse-integer timezone-string :start 4 :end 6)))
    (make-timezone (* sign hours) (* sign minutes))))

;; FIXME: this should be in local-time
(def function make-timezone (hours minutes)
  (let ((offset-in-sec (* (+ (* 60 hours) minutes) 60)))
      (if (and (= minutes 0)
               (= hours 0))
          +utc-zone+
          (local-time::make-timezone :subzones `((,offset-in-sec nil "anonymous" nil nil))
                         :name "anonymous"
                         :loaded t))))