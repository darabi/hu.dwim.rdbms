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
