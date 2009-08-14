;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.rdbms)

(define-syntax-node sql-index (named-sql-syntax-node)
  ((unique
    #f
    :type boolean)
   (table-name
    :type sql-identifier*)
   (columns
    nil
    :type list))
  (:documentation "An SQL index specification."))

(define-syntax-node sql-create-index (sql-ddl-statement sql-index)
  ()
  (:documentation "An SQL CREATE INDEX statement.")
  (:format-sql-syntax-node
   (format-string "CREATE ")
   (when unique
     (format-string "UNIQUE "))
   (format-string "INDEX ")
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
