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
  ())

(define-syntax-node sql-number-type (sql-simple-type)
  ())

(define-syntax-node sql-bit-sized-type (sql-type)
  ((bit-size
    nil
    :type (or null integer))))

(define-syntax-node sql-float-type (sql-bit-sized-type)
  ())

(define-syntax-node sql-integer-type (sql-bit-sized-type)
  ())

(define-syntax-node sql-varchar-type (sql-type)
  ((size
    nil
    :type (or null integer))))

(define-syntax-node sql-date-type (sql-simple-type)
  ())

(define-syntax-node sql-time-type (sql-simple-type)
  ())

(define-syntax-node sql-timestamp-type (sql-simple-type)
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
