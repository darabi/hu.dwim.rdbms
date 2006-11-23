;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms)

#.(file-header)

(defvar *sql-stream*)

(defclass* sql-syntax-node ()
  ()
  (:documentation "Base class for all kind of SQL syntax nodes."))

(defclass* sql-expression (sql-syntax-node)
  ()
  (:documentation "Base class for all top level SQL statements"))

(defun format-sql (expression &key (stream t) (database *database*))
  (let ((*sql-stream* stream)
        (*database* database))
    (format-sql-syntax-node expression database)
    (values)))

(defun format-sql-to-string (expression &rest args &key &allow-other-keys)
  (with-output-to-string (stream)
    (apply #'format-sql expression :stream stream args)))

(defgeneric format-sql-syntax-node (node database)
  (:documentation "Formats an SQL syntax tree into a string.")

  (:method ((name symbol) database)
           (write-string (symbol-name name) *sql-stream*)))
