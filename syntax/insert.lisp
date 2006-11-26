;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms)

#.(file-header)

(define-syntax-node sql-insert (sql-dml-statement)
  ((table
    :type sql-identifier*)
   (columns
    :type (list sql-identifier*))
   (values
    :type (list sql-literal*)))
  (:documentation "An SQL INSERT statement.")
  (:format-sql-syntax-node
   (format-string "INSERT INTO ")
   (format-sql-identifier table)
   (format-string " (")
   (format-comma-separated-identifiers columns)
   (format-string ") VALUES (")
   (format-comma-separated-list values)
   (format-char ")")))
