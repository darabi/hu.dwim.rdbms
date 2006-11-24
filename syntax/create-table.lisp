;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms)

#.(file-header)

(defclass* sql-create-table (sql-statement)
  ((name
    :type string)
   (temporary
    #f
    :type boolean)
   (columns
    nil
    :type list))
  (:documentation "An SQL CREATE TABLE statement."))

(defclass* sql-column (sql-syntax-node)
  ((name
    :type string)
   (type
    :type sql-type)
   (constraints
    nil
    :type list))
  (:documentation "An SQL column specification."))

(defmethod format-sql-syntax-node ((create-table sql-create-table) database)
  (write-string "CREATE" *sql-stream*)
  (when (temporary-p create-table)
    (write-string " TEMPORARY" *sql-stream*))
  (write-string " TABLE " *sql-stream*)
  (format-sql-syntax-node (name-of create-table) database)
  (write-string " (" *sql-stream*)
  (loop for i = nil then t
        for column in (columns-of create-table)
        when i
        do (write-string ", " *sql-stream*)
        do (format-sql-syntax-node column database))
  (write-char #\) *sql-stream*))

(defmethod format-sql-syntax-node ((column sql-column) database)
  (format-sql-syntax-node (name-of column) database)
  (write-char #\Space *sql-stream*)
  (format-sql-syntax-node (type-of column) database)
  (awhen (constraints-of column)
    (mapc (lambda (constraint) (format-sql-syntax-node constraint database)) it)))
