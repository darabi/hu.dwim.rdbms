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
    :type sql-identifier*)))

(define-syntax-node sql-binary-operator (sql-operator)
  ((left
    :type sql-expression)
   (right
    :type sql-expression))
  (:format-sql-syntax-node
   (format-sql-syntax-node left)
   (format-char " ")
   (format-sql-identifier (name-of self))
   (format-char " ")
   (format-sql-syntax-node right)))

(define-syntax-node sql-n-ary-operator (sql-operator)
  ((expressions
    :type list))
  (:format-sql-syntax-node
   (loop for i = nil then t
         for expression in expressions
         when i
         do (format-string (name-of self))
         do (format-sql-syntax-node expression))))

(define-syntax-node sql-function-call (sql-expression)
  ((name
    :type sql-identifier*)
   (arguments
    nil))
  (:format-sql-syntax-node
   (format-sql-identifier name)
   (format-char "(")
   (dolist (arg arguments)
     (format-sql-syntax-node arg))
   (format-char ")")))
