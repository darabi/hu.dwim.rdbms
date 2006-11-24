;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms)

#.(file-header)

(defclass* sql-select (sql-statement)
  ((table-aliases
    :type list)
   (column-aliases
    :type list)
   (where
    :type sql-where))
  (:documentation "An SQL SELECT statement."))

(defclass* sql-table-alias (sql-syntax-node)
  ((name
    :type string)
   (alias
    :type string)))

(defclass* sql-column-alias (sql-syntax-node)
  ((table-name
    :type string)
   (column-name
    :type string)
   (alias
    :type string)))

(defclass* sql-where (sql-syntax-node)
  ((expression
    :type sql-expression)))

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
  (format-sql-syntax-node (where-of select) database))

(defmethod format-sql-syntax-node ((alias sql-table-alias) database)
  (format-sql-syntax-node (table-name-of alias) database)
  (write-string " AS " *sql-stream*)
  (format-sql-syntax-node (alias-of alias) database))

(defmethod format-sql-syntax-node ((alias sql-column-alias) database)
  (format-sql-syntax-node (table-name-of alias) database)
  (write-char #\. *sql-stream*)
  (format-sql-syntax-node (column-name-of alias) database)
  (write-string " AS " *sql-stream*)
  (format-sql-syntax-node (alias-of alias) database))

(defmethod format-sql-syntax-node ((where sql-where) database)
  (write-string " WHERE " *sql-stream*)
  (format-sql-syntax-node (expression-of where) database))

#|
#SQL (select (_x a) (_x b) (_y c)
             :from (x _x) (y _y) (z _z)
             :where (= (_x a) (_y _b)))
|#