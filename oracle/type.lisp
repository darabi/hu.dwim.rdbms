;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms.oracle)

#.(file-header)

(defstruct typemap
  external-type
  lisp-to-oci
  oci-to-lisp)

;; TODO add a naming convention, maybe *foo*?
(defmacro deftypemap (name &rest args)
  `(defparameter ,name (make-typemap ,@args)))

(deftypemap boolean/char
    :external-type oci:+sqlt-afc+
    :lisp-to-oci #'boolean-to-char
    :oci-to-lisp #'boolean-from-char)

(deftypemap integer/int8
    :external-type oci:+sqlt-int+
    :lisp-to-oci #'integer-to-int8
    :oci-to-lisp #'integer-from-int8)

(deftypemap integer/int16
    :external-type oci:+sqlt-int+
    :lisp-to-oci #'integer-to-int16
    :oci-to-lisp #'integer-from-int16)

(deftypemap integer/int32
    :external-type oci:+sqlt-int+
    :lisp-to-oci #'integer-to-int32
    :oci-to-lisp #'integer-from-int32)

(deftypemap integer/varnum
    :external-type oci:+sqlt-vnu+
    :lisp-to-oci #'integer-to-varnum     ;; TODO: pass precision/scale
    :oci-to-lisp #'integer-from-varnum)

(deftypemap float/bfloat
    :external-type oci:+sqlt-bfloat+
    :lisp-to-oci #'float-to-bfloat
    :oci-to-lisp #'float-from-bfloat)

(deftypemap double/bdouble
    :external-type oci:+sqlt-bdouble+
    :lisp-to-oci #'double-to-bdouble
    :oci-to-lisp #'double-from-bdouble)

(deftypemap rational/varnum
    :external-type oci:+sqlt-vnu+
    :lisp-to-oci #'rational-to-varnum      ;; TODO: pass precision/scale
    :oci-to-lisp #'rational-from-varnum)

(deftypemap string/string
    :external-type oci:+sqlt-str+
    :lisp-to-oci #'string-to-string
    :oci-to-lisp #'string-from-string)

(deftypemap string/long-varchar
    :external-type oci:+sqlt-lvc+
    :lisp-to-oci #'string-to-long-varchar
    :oci-to-lisp #'string-from-long-varchar)

(deftypemap local-time/date
    :external-type oci:+sqlt-dat+
    :lisp-to-oci #'local-time-to-date
    :oci-to-lisp #'local-time-from-date)

(deftypemap local-time/oci-date
    :external-type oci:+sqlt-odt+
    :lisp-to-oci #'local-time-to-oci-date
    :oci-to-lisp #'local-time-from-oci-date)

(deftypemap local-time/timestamp
    :external-type oci:+sqlt-timestamp+
    :lisp-to-oci #'local-time-to-timestamp
    :oci-to-lisp #'local-time-from-timestamp)

(deftypemap local-time/timestamp-tz
    :external-type oci:+sqlt-timestamp-tz+
    :lisp-to-oci #'local-time-to-timestamp-tz
    :oci-to-lisp #'local-time-from-timestamp-tz)

(deftypemap byte-array/long-varraw
    :external-type oci:+sqlt-lvb+
    :lisp-to-oci #'byte-array-to-long-varraw
    :oci-to-lisp #'byte-array-from-long-varraw)


(defgeneric typemap-for-sql-type (type)
  
  (:method ((type sql-boolean-type))
           ;; booleans are stored as CHAR(1) internally
           boolean/char)

  (:method ((type sql-integer-type))
           ;; integers are stored as NUMBER(x) internally
           ;; where x=3  for 8-bit integers
           ;;       x=5  for 16-bit integers
           ;;       x=10 for 32-bit integers
           ;;       x=38 for bigger integers
           ;; their external type is byte/short/int or byte[22] (varnum format)
           ;; XXX OCI does not have external type for int64?
           (with-slots (bit-size) type
             (cond
               ((cl:null bit-size) integer/varnum)
               ((<= bit-size 8) integer/int8)
               ((<= bit-size 16) integer/int16)
               ((<= bit-size 32) integer/int32)
               (t integer/varnum))))

  (:method ((type sql-float-type))
           ;; floats are stored as BINARY_FLOAT or BINARY_DOUBLE internally
           ;; their external type is float/double
           (with-slots (bit-size) type
             (assert (and bit-size (<= 32 bit-size 64)))
             (cond
               ((<= bit-size 32) float/bfloat)
               ((<= bit-size 64) double/bdouble))))

  (:method ((type sql-numeric-type))
           ;; numeric values are stored as NUMBER internally
           ;; their external type is byte[22] (varnum)
           ;; NOTE: when rationals stored in a numeric column, their precision may be lost
           ;;       e.g. 1/3 -> 3333.../10000...
           rational/varnum)

  (:method ((type sql-character-type))
           ;; string values stored as CHAR(x) internally
           ;; their external format is zero terminated string
           string/string)

  (:method ((type sql-character-varying-type))
           ;; string values stored as VARCHAR2(x) internally
           ;; their external format is zero terminated string
           string/string)

  (:method ((type sql-character-large-object-type))
           ;; string values stored as CLOB internally
           ;; their external format is LONG VARCHAR allowing max 2^31-5 bytes
           string/long-varchar)

  (:method ((type sql-date-type))
           local-time/date)

  (:method ((type sql-time-type))
           local-time/timestamp)

  (:method ((type sql-timestamp-type))
           (if (with-timezone-p type)
               local-time/timestamp
               local-time/timestamp-tz))

  (:method ((type sql-binary-large-object-type))
           ;; binary values stored as BLOB internally
           ;; their external format is LONG VARRAW allowing max 2^31-5 bytes
           byte-array/long-varraw))


(defvar *oracle-database* (make-instance 'oracle)) ; TODO use class prototype

(defun internal-type-for-sql-type (type)
  (let ((str (format-sql-to-string type :database *oracle-database*)))
    (string-downcase
     (aif (position #\( str :test #'char=)
          (subseq str 0 it)             ; TODO ???
          str))))

(defun sql-type-for-internal-type (type)
  (declare (ignore type))
  nil) ; TODO


(defun external-type-for-sql-type (type)
  (typemap-external-type (typemap-for-sql-type type)))

(defun typemap-for-internal-type (internal-type size &key precision scale)
  (declare (fixnum internal-type))
  (ecase internal-type
    (#.oci:+sqlt-chr+ string/string)    ; varchar
    (#.oci:+sqlt-afc+                   ; char, boolean
     (if (= size 1)
         boolean/char    ; KLUDGE char(1) assumed to be a boolean
         string/string))
    (#.oci:+sqlt-num+
     (if (and (<= scale 0) (<= (- precision scale) 9))
         integer/int32
         rational/varnum))
    (#.oci:+sqlt-dat+ local-time/date)
    (#.oci:+sqlt-ibfloat+ float/bfloat)
    (#.oci:+sqlt-ibdouble+ double/bdouble)
    (180 local-time/timestamp)    ; timestamp
    (181 local-time/timestamp-tz) ; timestamp with timezone
    (#.oci:+sqlt-clob+ string/long-varchar)
    (#.oci:+sqlt-blob+ byte-array/long-varraw)))

(defun data-size-for (external-type column-size)
  (declare (fixnum external-type))
  (ecase external-type
    (#.oci:+sqlt-afc+ (* (oci-char-width) column-size))
    (#.oci:+sqlt-int+ 4)
    (#.oci:+sqlt-vnu+ 22)
    (#.oci:+sqlt-bfloat+ 4)
    (#.oci:+sqlt-bdouble+ 8)
    (#.oci:+sqlt-str+ (* (oci-char-width) (1+ column-size)))
    (#.oci:+sqlt-lvc+ (min (+ column-size 4) 8000)) ; FIXME
    (#.oci:+sqlt-dat+ 7)
    (#.oci:+sqlt-odt+ (cffi:foreign-type-size 'oci:date))
    (#.oci:+sqlt-timestamp+ (cffi:foreign-type-size 'oci:date-time))
    (#.oci:+sqlt-timestamp-tz+ (cffi:foreign-type-size 'oci:date-time))
    (#.oci:+sqlt-lvb+ (min (+ column-size 4) 8000)))) ; FIXME
