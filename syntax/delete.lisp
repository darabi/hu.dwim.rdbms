;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms)

#.(file-header)

(define-syntax-node sql-delete (sql-dml-statement)
  ((table-name
    :type string)
   (where
    :type sql-expression))
  (:documentation "An SQL DELETE statement."))

(defmethod format-sql-syntax-node ((delete sql-delete) database)
  (write-string "DELETE FROM " *sql-stream*)
  (format-sql-syntax-node (table-name-of delete) database)
  (awhen (where-of delete)
    (write-string " WHERE " *sql-stream*)
    (format-sql-syntax-node it database)))
