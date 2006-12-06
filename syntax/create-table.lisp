;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms)

#.(file-header)

(define-syntax-node sql-create-table (sql-ddl-statement)
  ((name
    :type string)
   (temporary
    nil
    :type (or null (member (:drop :preserve-rows :delete-rows))))
   (columns
    nil
    :type list))
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
   (when temporary
     (format-string " ON COMMIT ")
     (format-string (ecase temporary
                      (:drop "DROP")
                      (:preserve-rows "PRESERVE ROWS")
                      (:delete-rows "DELETE ROWS"))))))

(define-syntax-node sql-column (sql-syntax-node)
  ((name
    :type string)
   (type
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


