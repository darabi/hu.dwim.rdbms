;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms)

#.(file-header)

(defclass* sql-drop-table (sql-statement)
  ((table-name
    :type symbol))
  (:documentation "An SQL ALTER TABLE statement."))

(defmethod format-sql-syntax-node ((stmt sql-drop-table) database)
  (with-slots (table-name alter-table-action) stmt
    (write-string "DROP TABLE " *sql-stream*)
    (format-sql-syntax-node table-name database)))
