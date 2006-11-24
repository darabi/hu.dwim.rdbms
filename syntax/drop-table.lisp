;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms)

#.(file-header)

(define-syntax-node sql-drop-table (sql-statement)
  ((name
    :type string))
  (:documentation "An SQL ALTER TABLE statement."))

(defmethod format-sql-syntax-node ((drop-table sql-drop-table) database)
  (write-string "DROP TABLE " *sql-stream*)
  (format-sql-syntax-node (name-of drop-table) database))
