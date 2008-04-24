;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms.sqlite)

(defmethod format-sql-syntax-node ((type sql-integer-type) (database sqlite))
  (format-string "INTEGER"))

(defmethod format-sql-syntax-node ((type sql-float-type) (database sqlite))
  (format-string "REAL"))

(defmethod format-sql-syntax-node ((type sql-character-large-object-type) (database sqlite))
  (format-string "TEXT"))

(defmethod format-sql-syntax-node ((type sql-binary-large-object-type) (database sqlite))
  (format-string "BLOB"))
