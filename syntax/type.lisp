;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms)

#.(file-header)

(define-syntax-node sql-type (sql-syntax-node)
  ()
  (:documentation "Base class for all SQL types."))

(define-syntax-node sql-simple-type (sql-type)
  ())

(define-syntax-node sql-boolean-type (sql-simple-type)
  ()
  (:format-sql-syntax-node
   (format-string "BOOL")))

(define-syntax-node sql-number-type (sql-simple-type)
  ()
  (:format-sql-syntax-node
   (format-string "NUMERIC")))

(define-syntax-node sql-bit-sized-type (sql-type)
  ((bit-size
    nil
    :type (or null integer))))

(define-syntax-node sql-float-type (sql-bit-sized-type)
  ()
  (:format-sql-syntax-node
   (assert (and bit-size
                (<= 32 bit-size 64)))
   (cond ((<= bit-size 32)
          (format-string "FLOAT4"))
         ((<= bit-size 64)
          (format-string "FLOAT8")))))

(define-syntax-node sql-integer-type (sql-bit-sized-type)
  ()
  (:format-sql-syntax-node
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

(define-syntax-node sql-sized-type (sql-type)
  ((size
    nil
    :type (or null integer))))

(define-syntax-node sql-char-type (sql-sized-type)
  ()
  (:format-sql-syntax-node
   (format-string "CHAR")
   (when size
     (format-char "(")
     (format-number size)
     (format-char ")"))))

(define-syntax-node sql-varchar-type (sql-sized-type)
  ()
  (:format-sql-syntax-node
   (format-string "VARCHAR")
   (when size
     (format-char "(")
     (format-number size)
     (format-char ")"))))

(define-syntax-node sql-text-type (sql-simple-type)
  ()
  (:format-sql-syntax-node
   (format-string "TEXT")))

(define-syntax-node sql-date-type (sql-simple-type)
  ()
  (:format-sql-syntax-node
   (format-string "DATE")))

(define-syntax-node sql-time-type (sql-simple-type)
  ()
  (:format-sql-syntax-node
   (format-string "TIME")))

(define-syntax-node sql-timestamp-type (sql-simple-type)
  ()
  (:format-sql-syntax-node
   (format-string "TIMESTAMP")))

(defmethod equal-type-p ((type-1 sql-simple-type) (type-2 sql-simple-type) database)
  (eq (class-of type-1) (class-of type-2)))

(defmethod equal-type-p ((type-1 sql-bit-sized-type) (type-2 sql-bit-sized-type) database)
  (eq (bit-size-of type-1) (bit-size-of type-2)))

(defmethod equal-type-p ((type-1 sql-sized-type) (type-2 sql-sized-type) database)
  (eq (size-of type-1) (size-of type-2)))

(defmethod rdbms-type-for ((type sql-bit-sized-type) database)
  (let ((bit-size (bit-size-of type)))
    (cond ((null bit-size)
           (make-instance (class-of type)))
          ((<= bit-size 16)
           (make-instance (class-of type) :bit-size 16))
          ((<= bit-size 32)
           (make-instance (class-of type) :bit-size 32))
          ((<= bit-size 64)
           (make-instance (class-of type) :bit-size 64))
          (t
           (make-instance (class-of type))))))

(defparameter +the-sql-integer-16-type+ (make-instance 'sql-integer-type :bit-size 16))

(defparameter +the-sql-integer-32-type+ (make-instance 'sql-integer-type :bit-size 32))

(defparameter +the-sql-integer-64-type+ (make-instance 'sql-integer-type :bit-size 64))

(defparameter +the-sql-text-type+ (make-instance 'sql-text-type))
