;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms.postgresql)

#.(file-header)

(defmethod format-sql-syntax-node ((variable sql-binding-variable) (database oracle))
  (vector-push-extend variable *binding-entries*)
  (format-string ":")
  (format-string (princ-to-string (length *binding-entries*))))
