;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms)

#.(file-header)

;;;;;;;;;;;;;;;
;;; Expressions

(define-syntax-node sql-expression (sql-syntax-node)
  ())

;;;;;;;;;;;;;
;;; Operators

(define-syntax-node sql-operator (sql-expression)
  ((name
    :type sql-identifier*)))

(define-syntax-node sql-unary-operator (sql-operator)
  ((expression
    :type sql-expression))
  (:format-sql-syntax-node
   (format-sql-identifier (name-of self))
   (format-char " ")
   (format-sql-syntax-node expression)))

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
   (format-separated-list expressions (strcat " " (name-of self) " "))))

(defmacro define-unary-operator (name)
  `(defun ,(concatenate-symbol (find-package :cl-rdbms) "SQL-" name) (expression)
    (make-instance 'sql-unary-operator
     :name ,(string-upcase name)
     :expression expression)))

(defmacro define-binary-operator (name)
  `(defun ,(concatenate-symbol (find-package :cl-rdbms) "SQL-" name) (left right)
    (make-instance 'sql-binary-operator
     :name ,(string-upcase name)
     :left left
     :right right)))

(defmacro define-n-ary-operator (name)
  `(defun ,(concatenate-symbol (find-package :cl-rdbms) "SQL-" name) (&rest expressions)
    (make-instance 'sql-n-ary-operator
     :name ,(string-upcase name)
     :expressions expressions)))

;;;;;;;;;;;;;;;;;;;;;
;;; Logical operators

(define-unary-operator not)

(define-n-ary-operator and)

(define-n-ary-operator or)

;;;;;;;;;;;;;;;;;
;;; Set operators

(define-binary-operator in)

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Comparison operators

(define-binary-operator =)

(define-binary-operator <)

(define-binary-operator >)

(define-binary-operator <=)

(define-binary-operator >=)

(define-binary-operator <>)

;;;;;;;;;;;;;
;;; Functions

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

(defmacro define-aggregate-function (name)
  `(defun ,(concatenate-symbol (find-package :cl-rdbms) "SQL-" name) (&rest arguments)
    (make-instance 'sql-function-call
     :name ,(string-upcase name)
     :arguments arguments)))

;;;;;;;;;;;;;;;;;;;;;;;
;;; Aggregate functions

(define-aggregate-function min)

(define-aggregate-function max)

(define-aggregate-function avg)

(define-aggregate-function sum)
