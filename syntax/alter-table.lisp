;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms)

#.(file-header)

(defclass* sql-alter-table (sql-statement)
  ((name
    :type symbol)
   (actions
    :type list))
  (:documentation "An SQL ALTER TABLE statement."))

(defclass* sql-alter-table-action (sql-syntax-node)
  ())

(defclass* sql-alter-table-column-action (sql-alter-table-action)
  ((name
    :type symbol)
   (type
    :type sql-type)))

(defclass* sql-alter-table-add-column-action (sql-alter-table-column-action)
  ((default-value
    :type t)
   (constraints
    :type list)))

(defclass* sql-alter-table-drop-column-action (sql-alter-table-column-action)
  ())

(defclass* sql-alter-table-alter-column-type-action (sql-alter-table-column-action)
  ((constraints
    :type list)))

(defmethod format-sql-syntax-node ((alter-table sql-alter-table) database)
  (write-string "ALTER TABLE " *sql-stream*)
  (format-sql-syntax-node (name-of alter-table) database)
  (write-char #\Space *sql-stream*)
  (loop for i = nil then t
        for action in (actions-of alter-table)
        when i
        do (write-string ", " *sql-stream*)
        do (format-sql-syntax-node action database)))

(defmethod format-sql-syntax-node ((action sql-alter-table-add-column-action) database)
  (write-string "ADD (" *sql-stream*)
  (format-sql-syntax-node (name-of action) database)
  (write-char #\Space *sql-stream*)
  (format-sql-syntax-node (type-of action) database)
  (write-char #\) *sql-stream*))

(defmethod format-sql-syntax-node ((action sql-alter-table-drop-column-action) database)
  (write-string "DROP COLUMN " *sql-stream*)
  (format-sql-syntax-node (name-of action) database))

(defmethod format-sql-syntax-node ((action sql-alter-table-alter-column-type-action) database)
  (write-string "ALTER COLUMN " *sql-stream*)
  (format-sql-syntax-node (name-of action) database)
  (write-string " TYPE " *sql-stream*)
  (format-sql-syntax-node (type-of action) database))
