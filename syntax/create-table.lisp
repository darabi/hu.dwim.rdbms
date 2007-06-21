;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms)

#.(file-header)

(define-syntax-node sql-create-table (sql-ddl-statement)
  ((name
    :type sql-identifier*)
   (temporary
    nil
    :type (or boolean (member (:drop :preserve-rows :delete-rows))))
   (columns
    nil
    :type list)
   (as
    nil
    :type sql-subquery))
  (:documentation "An SQL CREATE TABLE statement.")
  (:format-sql-syntax-node
   (format-string "CREATE")
   (when temporary
     (format-string " TEMPORARY"))
   (format-string " TABLE ")
   (format-sql-identifier name)
   (format-string " (")
   (format-comma-separated-list columns)
   (format-char ")")
   (when as
     (format-string " AS ")
     (format-sql-syntax-node as))
   (when (and temporary (not (eq temporary #t)) (not as))
     (format-string " ON COMMIT ")
     (format-string (ecase temporary
                      (:drop "DROP")
                      (:preserve-rows "PRESERVE ROWS")
                      (:delete-rows "DELETE ROWS"))))))

(define-syntax-node sql-column (named-sql-syntax-node)
  ((type
    :type sql-type)
   (constraints
    nil
    :type list))
  (:documentation "An SQL column specification.")
  (:format-sql-syntax-node
   (format-sql-identifier name)
   (when type
     (format-char " ")
     (format-sql-syntax-node type))
   (mapc (lambda (constraint) (format-sql-syntax-node constraint)) constraints))
  (:format-sql-identifier
   (format-sql-identifier name)))
