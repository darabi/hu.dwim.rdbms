;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms.sqlite)

;;;----------------------------------------------------------------------------
;;; Literals
;;;

(defmethod format-sql-literal ((value (eql nil)) (database sqlite))
  (format-string "0"))

(defmethod format-sql-literal ((value (eql t)) (database sqlite))
  (format-string "1"))

(defmethod format-sql-literal ((literal sql-literal) (database sqlite))
  (if (unquote-aware-format-sql-literal literal)
      (progn
        (format-string ":")
        (format-string (princ-to-string (length *binding-types*))))
      (call-next-method)))

;;;----------------------------------------------------------------------------
;;; Bindings
;;;

(defmethod format-sql-syntax-node ((variable sql-binding-variable) (database sqlite))
  (unquote-aware-format-sql-binding-variable variable)
  (format-string ":")
  (format-string (princ-to-string (length *binding-types*))))
