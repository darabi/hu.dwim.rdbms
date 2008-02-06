;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms)

#.(file-header)

(define-syntax-node sql-alter-table (sql-ddl-statement)
  ((name
    :type string)
   (actions
    :type list))
  (:documentation "An SQL ALTER TABLE statement.")
  (:format-sql-syntax-node
   (format-string "ALTER TABLE ")
   (format-sql-identifier name)
   (format-char " ")
   (format-comma-separated-list actions)))

(define-syntax-node sql-add-column-action (sql-column)
  ((default-value
    :type t))
  (:format-sql-syntax-node
   (format-string "ADD (")
   (format-sql-identifier name)
   (format-char " ")
   (format-sql-syntax-node type)
   (format-char ")")))

(define-syntax-node sql-drop-column-action (sql-column)
  ()
  (:format-sql-syntax-node
   (format-string "DROP COLUMN ")
   (format-sql-identifier name)))

(define-syntax-node sql-alter-column-type-action (sql-column)
  ()
  (:format-sql-syntax-node
   (format-string "ALTER COLUMN ")
   (format-sql-identifier name)
   (format-string " TYPE ")
   (format-sql-syntax-node type)))

(define-syntax-node sql-add-constraint-action (sql-syntax-node)
  ()
  (:format-sql-syntax-node
   (format-string "ADD ")))

(define-syntax-node sql-add-primary-key-constraint-action (sql-add-constraint-action)
  ((columns
    :type list))
  (:format-sql-syntax-node
   (call-next-method)
   (format-string "PRIMARY KEY (")
   (format-comma-separated-identifiers columns)
   (format-string ")")))
