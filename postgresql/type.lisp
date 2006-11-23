;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms)

#.(file-header)

(defmethod format-sql-syntax-node ((type sql-boolean-type) (database postgresql))
  (write-string "BOOL" *sql-stream*))

(defmethod format-sql-syntax-node ((type sql-integer-type) (database postgresql))
  (let ((bit-size (bit-size-of type)))
    (cond ((<= bit-size 16)
           (write-string "INT2" *sql-stream*))
          ((<= bit-size 32)
           (write-string "INT4" *sql-stream*))
          ((<= bit-size 64)
           (write-string "INT8" *sql-stream*))
          (t
           (write-string "NUMERIC" *sql-stream*)))))
