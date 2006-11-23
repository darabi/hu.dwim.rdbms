;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms)

#.(file-header)

(defmethod format-sql-syntax-node ((type sql-int-16-type) (database postgresql))
  (write-string "INT2" *sql-stream*))

(defmethod format-sql-syntax-node ((type sql-int-32-type) (database postgresql))
  (write-string "INT4" *sql-stream*))

(defmethod format-sql-syntax-node ((type sql-int-64-type) (database postgresql))
  (write-string "INT8" *sql-stream*))
