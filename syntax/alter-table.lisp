;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms)

#.(file-header)

(defclass* sql-alter-table (sql-ddl-statement)
  ((name
    :type string)
   (actions
    :type list))
  (:documentation "An SQL ALTER TABLE statement."))

(defclass* sql-add-column-action (sql-column)
  ((default-value
    :type t)))

(defclass* sql-drop-column-action (sql-column)
  ())

(defclass* sql-alter-column-type-action (sql-column)
  ())

(defmethod format-sql-syntax-node ((alter-table sql-alter-table) database)
  (write-string "ALTER TABLE " *sql-stream*)
  (format-sql-syntax-node (name-of alter-table) database)
  (write-char #\Space *sql-stream*)
  (loop for i = nil then t
        for action in (actions-of alter-table)
        when i
        do (write-string ", " *sql-stream*)
        do (format-sql-syntax-node action database)))

(defmethod format-sql-syntax-node ((action sql-add-column-action) database)
  (write-string "ADD (" *sql-stream*)
  (format-sql-syntax-node (name-of action) database)
  (write-char #\Space *sql-stream*)
  (format-sql-syntax-node (type-of action) database)
  (write-char #\) *sql-stream*))

(defmethod format-sql-syntax-node ((action sql-drop-column-action) database)
  (write-string "DROP COLUMN " *sql-stream*)
  (format-sql-syntax-node (name-of action) database))

(defmethod format-sql-syntax-node ((action sql-alter-column-type-action) database)
  (write-string "ALTER COLUMN " *sql-stream*)
  (format-sql-syntax-node (name-of action) database)
  (write-string " TYPE " *sql-stream*)
  (format-sql-syntax-node (type-of action) database))
