;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms)

#.(file-header)

(defvar *sql-stream*)

(defclass* sql-syntax-node ()
  ())

(defclass* sql-expression (sql-syntax-node)
  ())

(defun format-sql (expression &key (stream t) (database *database*))
  (let ((*sql-stream* stream)
        (*database* database))
    (format-sql-syntax-node expression database)
    (values)))

(defun format-sql-to-string (expression &rest args &key &allow-other-keys)
  (with-output-to-string (stream)
    (apply #'format-sql expression :stream stream args)))

(defgeneric format-sql-syntax-node (node database)
  (:method ((name symbol) database)
           (write-string (symbol-name name) *sql-stream*)))




;; TODO: move!

(defclass* sql-type (sql-syntax-node)
  ())

(defclass* sql-int4-type (sql-type)
  ())

(defmethod format-sql-syntax-node ((type sql-int4-type) database)
  (write-string "INT4" *sql-stream*))