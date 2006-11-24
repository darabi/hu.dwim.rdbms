;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms)

#.(file-header)

(define-syntax-node sql-insert (sql-dml-statement)
  ((table-name
    :type string)
   (columns
    :type list)
   (values
    :type list))
  (:documentation "An SQL INSERT statement."))

(defmethod format-sql-syntax-node ((insert sql-insert) database)
  (write-string "INSERT INTO " *sql-stream*)
  (format-sql-syntax-node (table-name-of insert) database)
  (write-string " (" *sql-stream*)
  (loop for i = nil then t
        for column in (columns-of insert)
        when i
        do (write-string ", " *sql-stream*)
        do (format-sql-syntax-node column database))
  (write-string ") VALUES (" *sql-stream*)
  (loop for i = nil then t
        for value in (values-of insert)
        when i
        do (write-string ", " *sql-stream*)
        do (format-sql-syntax-node value database))
  (write-char #\) *sql-stream*))
