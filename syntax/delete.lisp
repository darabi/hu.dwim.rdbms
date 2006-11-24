;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms)

#.(file-header)

(defclass* sql-delete (sql-statement)
  ((table-name
    :type string)
   (where
    :type sql-where))
  (:documentation "An SQL DELETE statement."))

(defmethod format-sql-syntax-node ((delete sql-delete) database)
  (write-string "DELETE FROM " *sql-stream*)
  (format-sql-syntax-node (table-name-of delete) database)
  (format-sql-syntax-node (where-of delete) database))
