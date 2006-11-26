;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms)

#.(file-header)

(define-syntax-node sql-update (sql-dml-statement)
  ((table
       :type sql-identifier*)
   (columns
    :type (list sql-identifier*))
   (values
    :type (list sql-literal*))
   (where
    nil
    :type sql-expression))
  (:documentation "An SQL UPDATE statement.")
  (:format-sql-syntax-node
   (format-string "UPDATE ")
   (format-sql-identifier table)
   (format-string " SET ")
   (loop for i = nil then t
         for column in columns
         for value in values
         when i
         do (format-string ", ")
         do
         (format-sql-identifier column)
         (format-string " = ")
         (format-sql-syntax-node value))
   (format-where where)))
