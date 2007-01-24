;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms)

#.(file-header)

(define-syntax-node sql-create-index (sql-ddl-statement)
  ((name
    :type sql-identifier*)
   (table-name
    :type sql-identifier*)
   (columns
    nil
    :type list))
  (:documentation "An SQL CREATE INDEX statement.")
  (:format-sql-syntax-node
   (format-string "CREATE INDEX ")
   (format-sql-identifier name)
   (format-string " ON ")
   (format-sql-identifier table-name)
   (format-string " (")
   (format-comma-separated-identifiers columns)
   (format-char ")")))

(define-syntax-node sql-drop-index (sql-ddl-statement)
  ((name
    :type string))
  (:documentation "An SQL DROP INDEX statement.")
  (:format-sql-syntax-node
    (format-string "DROP INDEX ")
    (format-sql-identifier name)))
