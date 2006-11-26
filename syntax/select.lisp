;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms)

#.(file-header)

(define-syntax-node sql-select (sql-dml-statement)
  ((table-aliases
    :type list)
   (column-aliases
    :type list)
   (where
    nil
    :type sql-expression))
  (:documentation "An SQL SELECT statement."))

(defmethod format-sql-syntax-node ((select sql-select) database)
  (write-string "SELECT " *sql-stream*)
  (loop for i = nil then t
        for column-alias in (column-aliases-of select)
        when i
        do (write-string ", " *sql-stream*)
        do (format-sql-syntax-node column-alias database))
  (write-string " FROM " *sql-stream*)
  (loop for i = nil then t
        for table-alias in (table-aliases-of select)
        when i
        do (write-string ", " *sql-stream*)
        do (format-sql-syntax-node table-alias database))
  (awhen (where-of select)
    (write-string " WHERE " *sql-stream*)
    (format-sql-syntax-node it database)))

(define-syntax-node sql-table-alias (sql-syntax-node)
  ((name
    :type string)
   (alias nil
          :type string)))

(defmethod format-sql-syntax-node ((alias sql-table-alias) database)
  (format-sql-syntax-node (name-of alias) database)
  (awhen (alias-of alias)
    (write-string " AS " *sql-stream*)
    (format-sql-syntax-node it database)))

(define-syntax-node sql-column-alias (sql-syntax-node)
  ((table-name
    nil
    :type string)
   (column-name
    :type string)
   (alias
    nil
    :type string)))

(defmethod format-sql-syntax-node ((alias sql-column-alias) database)
  (awhen (table-name-of alias)
    (format-sql-syntax-node it database)
    (write-char #\. *sql-stream*))
  (format-sql-syntax-node (column-name-of alias) database)
  (awhen (alias-of alias)
    (write-string " AS " *sql-stream*)
    (format-sql-syntax-node it database)))

(define-syntax-node sql-all-columns (sql-syntax-node)
  ())

(defmethod format-sql-syntax-node ((column sql-all-columns) database)
  (write-string " * " *sql-stream*))
