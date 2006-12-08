;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms)

#.(file-header)

(defmethod format-sql-syntax-node ((action sql-add-column-action) (database postgresql))
  (format-string "ADD ")
  (format-sql-identifier (name-of action) database)
  (format-char " ")
  (format-sql-syntax-node (type-of action) database))

(defmethod format-sql-literal ((literal sql-literal) (database postgresql))
  (if (type-of literal)
      (progn
        (push literal *binding-entries*)
        (format-string "$")
        (format-string (princ-to-string (length *binding-entries*))))
      (call-next-method)))

(defmethod format-sql-syntax-node ((variable sql-binding-variable) (database postgresql))
  (push variable *binding-entries*)
  (format-string "$")
  (format-string (princ-to-string (length *binding-entries*))))

