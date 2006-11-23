;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms)

#.(file-header)

(defclass* sql-alter-table (sql-statement)
  ((table-name
    :type symbol)
   (alter-table-action
    :type sql-alter-table-action))
  (:documentation "An SQL ALTER TABLE statement."))

(defclass* sql-alter-table-action (sql-syntax-node)
  ())

(defclass* sql-alter-table-column-action (sql-alter-table-action)
  ((column-name
    :type symbol)
   (column-type
    :type sql-column-type)))

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

(defmethod format-sql-syntax-node ((stmt sql-alter-table) database)
  (with-slots (table-name alter-table-action) stmt
    (write-string "ALTER TABLE " *sql-stream*)
    (format-sql-syntax-node table-name database)
    (write-char #\Space *sql-stream*)
    (format-sql-syntax-node alter-table-action database)))

;; TODO: output column constraints
(defmethod format-sql-syntax-node ((action sql-alter-table-add-column-action) database)
  (with-slots (column-name column-type) action
    (write-string "ADD " *sql-stream*)
    (format-sql-syntax-node column-name database)
    (write-char #\Space *sql-stream*)
    (format-sql-syntax-node column-type database)))

(defmethod format-sql-syntax-node ((action sql-alter-table-drop-column-action) database)
  (with-slots (column-name) action
    (write-string "DROP COLUMN " *sql-stream*)
    (format-sql-syntax-node column-name database)))

(defmethod format-sql-syntax-node ((action sql-alter-table-alter-column-type-action)
		       database)
  (with-slots (column-name column-type) action
    (write-string "ALTER COLUMN " *sql-stream*)
    (format-sql-syntax-node column-name database)
    (write-string " TYPE " *sql-stream*)
    (format-sql-syntax-node column-type database)))
