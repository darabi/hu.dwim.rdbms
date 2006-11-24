;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms)

#.(file-header)

(defclass* sql-type (sql-syntax-node)
  ()
  (:documentation "Base class for all SQL types."))

(defclass* sql-simple-type (sql-type)
  ())

(defclass* sql-boolean-type (sql-simple-type)
  ())

(defclass* sql-number-type (sql-simple-type)
  ())

(defclass* sql-bit-sized-type (sql-type)
  ((bit-size
    nil
    :type (or null integer))))

(defclass* sql-float-type (sql-bit-sized-type)
  ())

(defclass* sql-integer-type (sql-bit-sized-type)
  ())

(defclass* sql-varchar-type (sql-type)
  ((size
    nil
    :type (or null integer))))

(defclass* sql-date-type (sql-simple-type)
  ())

(defclass* sql-time-type (sql-simple-type)
  ())

(defclass* sql-timestamp-type (sql-simple-type)
  ())

(defmethod equal-type-p ((type-1 sql-simple-type) (type-2 sql-simple-type) database)
  (eq (class-of type-1) (class-of type-2)))

(defmethod equal-type-p ((type-1 sql-bit-sized-type) (type-2 sql-bit-sized-type) database)
  (eq (bit-size-of type-1) (bit-size-of type-2)))

(defmethod equal-type-p ((type-1 sql-varchar-type) (type-2 sql-varchar-type) database)
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
