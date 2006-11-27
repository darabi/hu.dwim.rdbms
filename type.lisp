;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms)

#.(file-header)

(defmethod format-sql-syntax-node ((type sql-boolean-type) database)
  (format-string "BOOL"))

(defmethod format-sql-syntax-node ((type sql-number-type) database)
  (format-string "NUMERIC"))

(defmethod format-sql-syntax-node ((type sql-integer-type) database)
  (let ((bit-size (bit-size-of type)))
    (cond ((null bit-size)
           (format-string "NUMERIC"))
          ((<= bit-size 16)
           (format-string "INT2"))
          ((<= bit-size 32)
           (format-string "INT4"))
          ((<= bit-size 64)
           (format-string "INT8"))
          (t
           (format-string "NUMERIC")))))

(defmethod format-sql-syntax-node ((type sql-float-type) database)
  (let ((bit-size (bit-size-of type)))
    (assert (and bit-size
                 (<= 32 bit-size 64)))
    (cond ((<= bit-size 32)
           (format-string "FLOAT4"))
          ((<= bit-size 64)
           (format-string "FLOAT8")))))

(defmethod format-sql-syntax-node ((type sql-varchar-type) database)
  (format-string "VARCHAR")
  (awhen (size-of type)
    (format-char "(")
    (format-number it)
    (format-char ")")))

(defmethod format-sql-syntax-node ((type sql-date-type) database)
  (format-string "DATE"))

(defmethod format-sql-syntax-node ((type sql-time-type) database)
  (format-string "TIME"))

(defmethod format-sql-syntax-node ((type sql-timestamp-type) database)
  (format-string "TIMESTAMP"))
