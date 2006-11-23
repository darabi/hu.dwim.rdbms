;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms)

#.(file-header)

(defmethod format-sql-syntax-node ((action sql-alter-table-add-column-action) (database postgresql))
  (write-string "ADD " *sql-stream*)
  (format-sql-syntax-node (name-of action) database)
  (write-char #\Space *sql-stream*)
  (format-sql-syntax-node (type-of action) database))
