;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms)

#.(file-header)

(defmethod format-sql-syntax-node ((action sql-alter-table-add-column-action) (database postgresql))
  (with-slots (column-name column-type) action
    (write-string "ADD " *sql-stream*)
    (format-sql-syntax-node column-name database)
    (write-char #\Space *sql-stream*)
    (format-sql-syntax-node column-type database)))
