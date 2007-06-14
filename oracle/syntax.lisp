;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms.oracle)

#.(file-header)

(defmethod format-sql-literal ((literal vector) (database oracle))
  (format-string "to_Blob('")
  (loop for el across literal
        do (format t "~2,'0x" el))
  (format-string "')"))

(defmethod format-sql-syntax-node ((variable sql-binding-variable) (database oracle))
  (vector-push-extend variable *binding-entries*)
  (format-string ":")
  (format-string (princ-to-string (length *binding-entries*))))

(defmethod format-sql-syntax-node ((self sql-boolean-type) (database oracle))
  (format-string "CHAR(1)"))

(defmethod format-sql-syntax-node ((self sql-float-type) (database oracle))
  (with-slots (bit-size) self
    (assert (and bit-size (<= 32 bit-size 64)))
    (cond
      ((<= bit-size 32) (format-string "BINARY_FLOAT"))
      ((<= bit-size 64) (format-string "BINARY_DOUBLE")))))

(defmethod format-sql-syntax-node ((self sql-integer-type) (database oracle))
  (with-slots (bit-size) self
    (cond
      ((cl:null bit-size) (format-string "NUMBER"))
      ((<= bit-size 16) (format-string "NUMBER(5)"))
      ((<= bit-size 32) (format-string "NUMBER(10)"))
      ((<= bit-size 64) (format-string "NUMBER(19)"))
      (t (format-string "NUMBER")))))

(defmethod format-sql-syntax-node ((self sql-character-varying-type) (database oracle))
  (with-slots (size) self
    (format-string "VARCHAR2")
    (format-size size)))

(defmethod format-sql-syntax-node ((self sql-character-large-object-type) (database oracle))
  (format-string "CLOB")) ; size ignored

(defmethod format-sql-syntax-node ((self sql-time-type) (database oracle))
  (format-string "TIMESTAMP"))

(defmethod format-sql-syntax-node ((self sql-interval-type) (database oracle))
  (error "sql-interval-type not yet supported"))

(defmethod format-sql-syntax-node ((self sql-binary-large-object-type) (database oracle))
  (format-string "BLOB")) ; size ignored




