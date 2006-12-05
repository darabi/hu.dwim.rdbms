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

(define-syntax-node sql-query-expression (sql-expression sql-dml-statement)
  ())

;;;;;;;;;;;;;;;;;;
;;; Set operations

(define-syntax-node sql-set-operation-expression (sql-query-expression)
  ((set-operation
    :type (member :union :except :intersect))
   (all
    #f
    :type boolean)
   (subqueries
    nil
    :type (list sql-query-expression)))
  (:format-sql-syntax-node
   (format-char "(")
   (format-separated-list subqueries
                          (strcat " " (symbol-name set-operation) (if all " ALL " " ")))
   (format-char ")")))

(defmacro define-set-operation (name)
  (let ((constructor-name (sql-constructor-name name)))
    `(progn
      (pushnew ',constructor-name *sql-constructor-names*)
      (defun ,constructor-name (&rest subqueries)
        (make-instance 'sql-set-operation-expression
                       :set-operation ,(intern (symbol-name name) (find-package :keyword))
                       :subqueries subqueries)))))

(define-set-operation union)

(define-set-operation except)

(define-set-operation intersect)

;;;;;;;;;;;;;
;;; Operators

(define-syntax-node sql-operator (sql-expression)
  ((name
    :type sql-identifier*)))

(define-syntax-node sql-unary-operator (sql-operator)
  ((expression
    :type sql-expression))
  (:format-sql-syntax-node
   (format-char "(")
   (format-sql-identifier name)
   (format-char " ")
   (format-sql-syntax-node expression)
   (format-char ")")))

(define-syntax-node sql-binary-operator (sql-operator)
  ((left
    :type sql-expression)
   (right
    :type sql-expression))
  (:format-sql-syntax-node
   (format-char "(")
   (format-sql-syntax-node left)
   (format-char " ")
   (format-sql-identifier name)
   (format-char " ")
   (format-sql-syntax-node right)
   (format-char ")")))

(define-syntax-node sql-n-ary-operator (sql-operator)
  ((expressions
    :type list))
  (:format-sql-syntax-node
   (format-char "(")
   (format-separated-list expressions (strcat " " name " "))
   (format-char ")")))

(defmacro define-unary-operator (name)
  (let ((constructor-name (sql-constructor-name name)))
    `(progn
      (pushnew ',constructor-name *sql-constructor-names*)
      (defun ,constructor-name (expression)
        (make-instance 'sql-unary-operator
                       :name ,(string-upcase name)
                       :expression expression)))))

(defmacro define-binary-operator (name)
  (let ((constructor-name (sql-constructor-name name)))
    `(progn
      (pushnew ',constructor-name *sql-constructor-names*)
      (defun ,constructor-name (left right)
        (make-instance 'sql-binary-operator
                       :name ,(string-upcase name)
                       :left left
                       :right right)))))

(defmacro define-n-ary-operator (name)
  (let ((constructor-name (sql-constructor-name name)))
    `(progn
      (pushnew ',constructor-name *sql-constructor-names*)
      (defun ,constructor-name (&rest expressions)
        (make-instance 'sql-n-ary-operator
                       :name ,(string-upcase name)
                       :expressions expressions)))))

(defmacro define-varary-operator (name)
  (let ((constructor-name (sql-constructor-name name)))
    `(progn
      (pushnew ',constructor-name *sql-constructor-names*)
      (defun ,constructor-name (&rest expressions)
        (if (length=1 expressions)
            (make-instance 'sql-unary-operator
                           :name ,(string-upcase name)
                           :expression (first expressions))
            (make-instance 'sql-n-ary-operator
                           :name ,(string-upcase name)
                           :expressions expressions))))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Arithmetic operators

(define-varary-operator +)

(define-varary-operator -)

(define-n-ary-operator *)

(define-binary-operator /)

(define-binary-operator %)

(define-binary-operator ^)

(define-n-ary-operator \|\|)

(define-unary-operator \|/)

(define-unary-operator @)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Pattern matching

(define-binary-operator like)

(define-binary-operator ~)

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Subquery expressions

(define-syntax-node sql-subquery (sql-query-expression)
  ((query
    :type sql-select)) ;; TODO: extract query-expression from the ddl statement
  (:format-sql-syntax-node
   (format-char "(")
   (format-sql-syntax-node query)
   (format-char ")")))

(define-unary-operator exists)

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
   (format-comma-separated-list arguments)
   (format-char ")")))

(defmacro define-aggregate-function (name)
  (let ((constructor-name (sql-constructor-name name)))
    `(progn
      (pushnew ',constructor-name *sql-constructor-names*)
      (defun ,constructor-name (&rest arguments)
        (make-instance 'sql-function-call
                       :name ,(string-upcase name)
                       :arguments arguments)))))

;;;;;;;;;;;;;;;;;;;;;;;
;;; Aggregate functions

(define-aggregate-function count)

(define-aggregate-function min)

(define-aggregate-function max)

(define-aggregate-function avg)

(define-aggregate-function sum)

;;;;;;;;;;;;
;;; Count(*)

(defun sql-count-* ()
  (sql-count (sql-identifier :name '*)))
