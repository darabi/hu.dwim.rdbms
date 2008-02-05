;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms)

#.(file-header)

(define-syntax-node sql-drop-table (sql-ddl-statement)
  ((name
    :type string))
  (:documentation "An SQL DROP TABLE statement.")
  (:format-sql-syntax-node
    (format-string "DROP TABLE ")
    (format-sql-identifier name)))

(define-syntax-node sql-drop-view (sql-ddl-statement)
  ((name
    :type string))
  (:documentation "An SQL DROP VIEW  statement.")
  (:format-sql-syntax-node
    (format-string "DROP VIEW ")
    (format-sql-identifier name)))
