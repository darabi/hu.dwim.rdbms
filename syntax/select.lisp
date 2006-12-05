;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms)

#.(file-header)

;; TODO: rename columns and tables to something more general?
(define-syntax-node sql-select (sql-dml-statement)
  ((distinct
    nil
    :type boolean)
   (columns
    :type (list sql-column-alias*))
   (tables
    nil
    :type (list sql-table-alias*))
   (where
    nil
    :type sql-expression)
   (order-by
    nil
    :type list)  ; TODO: element type
   (offset
    nil
    :type number)
   (limit
    nil
    :type number))
  (:documentation "An SQL SELECT statement.")
  (:format-sql-syntax-node
   (format-string "SELECT ")
   (when distinct
     (format-string "DISTINCT "))
   (format-comma-separated-identifiers columns)
   (when tables
     (format-string " FROM ")
     (format-comma-separated-identifiers tables))
   (format-where where)
   (when order-by
     (format-string " ORDER BY ")
     (format-comma-separated-list order-by))
   (when limit
     (format-string " LIMIT ")
     (format-number limit))
   (when offset
     (format-string " OFFSET ")
     (format-number offset))))

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

(define-syntax-node sql-all-columns (sql-identifier)
  ()
  (:format-sql-identifier
   (format-char "*")))

(define-syntax-node sql-sort-spec (sql-syntax-node)
  ((sort-key
    :type (or number sql-column-alias*))
   (ordering
    :ascending
    :type (member :ascending :descending)))
  (:format-sql-syntax-node
   (format-sql-syntax-node sort-key)
   (format-char " ")
   (ecase ordering
     (:ascending (format-string "ASC"))
     (:descending (format-string "DESC")))))
