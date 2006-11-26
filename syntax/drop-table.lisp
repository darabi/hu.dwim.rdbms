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
  (:documentation "An SQL ALTER TABLE statement.")
  (:format-sql-syntax-node
    (format-string "DROP TABLE ")
    (format-sql-identifier name)))
