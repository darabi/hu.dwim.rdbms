;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms)

#.(file-header)

(define-syntax-node sql-update (sql-dml-statement)
  ((table-name
    :type string)
   (column-names
    :type list)
   (values
    :type list)
   (where
    nil
    :type sql-where))
  (:documentation "An SQL UPDATE statement."))

(defmethod format-sql-syntax-node ((update sql-update) database)
  (write-string "UPDATE  " *sql-stream*)
  (format-sql-syntax-node (table-name-of update) database)
  (write-string " SET " *sql-stream*)
  (loop for i = nil then t
        for column-name in (column-names-of update)
        for value in (values-of update)
        when i
        do (write-string ", " *sql-stream*)
        do
        (format-sql-syntax-node column-name database)
        (write-string " = " *sql-stream*)
        (format-sql-syntax-node value database))
  (format-sql-syntax-node (where-of update) database))
