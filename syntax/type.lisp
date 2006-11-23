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

(defclass* sql-integer-type (sql-simple-type)
  ((bit-size
    nil
    :type integer)))

(defmethod equal-type-p ((type-1 sql-simple-type) (type-2 sql-simple-type) database)
  (eq (class-of type-1) (class-of type-2)))

(defmethod equal-type-p ((type-1 sql-integer-type) (type-2 sql-integer-type) database)
  (eq (bit-size-of type-1) (bit-size-of type-2)))

(defmethod rdbms-type-for ((type sql-integer-type) database)
  (let ((bit-size (bit-size-of type)))
    (cond ((<= bit-size 16)
           (make-instance 'sql-integer-type :bit-size 16))
          ((<= bit-size 32)
           (make-instance 'sql-integer-type :bit-size 32))
          ((<= bit-size 64)
           (make-instance 'sql-integer-type :bit-size 64))
          (t
           (make-instance 'sql-integer-type)))))
