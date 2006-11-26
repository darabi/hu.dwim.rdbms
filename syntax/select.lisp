;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms)

#.(file-header)

(define-syntax-node sql-select (sql-dml-statement)
  ((columns
    :type (list sql-column-alias*))
   (tables
    :type (list sql-table-alias*))
   (where
    nil
    :type sql-expression))
  (:documentation "An SQL SELECT statement.")
  (:format-sql-syntax-node
   (format-string "SELECT ")
   (format-comma-separated-identifiers columns)
   (format-string " FROM ")
   (format-comma-separated-identifiers tables)
   (format-where where)))

(define-syntax-node sql-table-alias (sql-identifier)
  ((name
    :type sql-identifier*)
   (alias
    nil
    :type sql-identifier*))
  (:format-sql-identifier
   (format-sql-identifier name)
   (when alias
     (format-string " ")
     (format-sql-identifier alias))))

(deftype sql-table-alias* ()
  '(or string symbol sql-table-alias))

(define-syntax-node sql-column-alias (sql-identifier)
  ((table
    nil
    :type sql-identifier*)
   (column
    :type sql-identifier*)
   (alias
    nil
    :type sql-identifier*))
  (:format-sql-identifier
   (awhen table
     (format-sql-identifier table)
     (format-char "."))
   (format-sql-identifier column)
   (when alias
     (format-string " AS ")
     (format-sql-identifier alias))))

(deftype sql-column-alias* ()
  '(or string symbol sql-column-alias))

(define-syntax-node sql-all-columns (sql-syntax-node)
  ()
  (:format-sql-identifier
   (format-char "*")))
