;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms)

#.(file-header)

(define-syntax-node sql-expression (sql-syntax-node)
  ())

(define-syntax-node sql-operator (sql-expression)
  ((name
    :type symbol)))

(define-syntax-node sql-binary-operator (sql-operator)
  ((left
    :type sql-expression)
   (right
    :type sql-expression)))

(define-syntax-node sql-n-ary-operator (sql-operator)
  ((expressions
    :type list)))

(defmethod format-sql-syntax-node ((operator sql-binary-operator) database)
  (format-sql-syntax-node (left-of operator) database)
  (write-char #\Space *sql-stream*)
  (write-string (string (name-of operator)) *sql-stream*)
  (write-char #\Space *sql-stream*)
  (format-sql-syntax-node (right-of operator) database))

(defmethod format-sql-syntax-node ((operator sql-n-ary-operator) database)
  (loop for i = nil then t
        for expression in (expressions-of operator)
        when i
        do (write-string (name-of operator) *sql-stream*)
        do (format-sql-syntax-node expression database)))

(define-syntax-node sql-function-call (sql-expression)
  ((name)
   (arguments nil)))

(defmethod format-sql-syntax-node ((function sql-function-call) database)
  (write-string (name-of function) *sql-stream*)
  (write-char #\( *sql-stream*)
  (dolist (arg (arguments-of function))
    (format-sql-syntax-node arg database))
  (write-char #\) *sql-stream*))