;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms)

#.(file-header)

(defmethod format-sql-syntax-node ((type sql-boolean-type) database)
  (format-string "bool"))

(defmethod format-sql-syntax-node ((type sql-number-type) database)
  (format-string "numeric"))

(defmethod format-sql-syntax-node ((type sql-integer-type) database)
  (let ((bit-size (bit-size-of type)))
    (cond ((null bit-size)
           (format-string "numeric"))
          ((<= bit-size 16)
           (format-string "int2"))
          ((<= bit-size 32)
           (format-string "int4"))
          ((<= bit-size 64)
           (format-string "int8"))
          (t
           (format-string "numeric")))))

(defmethod format-sql-syntax-node ((type sql-float-type) database)
  (let ((bit-size (bit-size-of type)))
    (assert (and bit-size
                 (<= 32 bit-size 64)))
    (cond ((<= bit-size 32)
           (format-string "float4"))
          ((<= bit-size 64)
           (format-string "float8")))))

(defmethod format-sql-syntax-node ((type sql-varchar-type) database)
  (format-string "varchar")
  (awhen (size-of type)
    (format-char "(")
    (format-number it)
    (format-char ")")))

(defmethod format-sql-syntax-node ((type sql-date-type) database)
  (format-string "date"))

(defmethod format-sql-syntax-node ((type sql-time-type) database)
  (format-string "time"))

(defmethod format-sql-syntax-node ((type sql-timestamp-type) database)
  (format-string "timestamp"))

;; TODO this may or may not be a generic enough protocol. for now it's postgresql only...
(defgeneric binding-type-for-sql-type (sql-type database))

(defparameter +the-sql-integer-16-type+ (make-instance 'sql-integer-type :bit-size 16))
(defparameter +the-sql-integer-32-type+ (make-instance 'sql-integer-type :bit-size 32))
(defparameter +the-sql-integer-64-type+ (make-instance 'sql-integer-type :bit-size 64))

(defparameter +the-sql-varchar-type+ (make-instance 'sql-varchar-type))
