;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms)

#.(file-header)

(defclass* sql-expression (sql-syntax-node)
  ())

(defclass* sql-operator (sql-expression)
  ((name
    :type symbol)))

(defclass* sql-binary-operator (sql-operator)
  ((left
    :type sql-expression)
   (right
    :type sql-expression)))

(defclass* sql-n-ary-operator (sql-operator)
  ((expressions
    :type list)))

(defmethod format-sql-syntax-node ((operator sql-binary-operator) database)
  (format-sql-syntax-node (left-of operator) database)
  (write-char #\Space *sql-stream*)
  (write-string (name-of operator) *sql-stream*)
  (write-char #\Space *sql-stream*)
  (format-sql-syntax-node (right-of operator) database))

(defmethod format-sql-syntax-node ((operator sql-n-ary-operator) database)
  (loop for i = nil then t
        for expression in (expressions-of operator)
        when i
        do (write-string (name-of operator) *sql-stream*)
        do (format-sql-syntax-node expression database)))
