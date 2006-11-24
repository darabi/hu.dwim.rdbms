;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms)

#.(file-header)

(defparameter *sql-syntax-node-names* nil)

(defun import-sql-syntax-node-names (&optional (package *package*))
  (import *sql-syntax-node-names* package))

(defvar *sql-stream*)

(defclass* sql-syntax-node ()
  ()
  (:documentation "Base class for all kind of SQL syntax elements."))

(defclass* sql-statement (sql-syntax-node)
  ()
  (:documentation "Base class for all top level SQL statements which can be executed."))

(defmacro define-syntax-node (name supers slots &rest options)
  `(progn
    (defclass* ,name ,supers ,slots
               ,@options)
    (pushnew ',name *sql-syntax-node-names*)))

(defun format-sql (statement &key (stream t) (database *database*))
  (let ((*sql-stream* stream)
        (*database* database))
    (format-sql-syntax-node statement database)
    (values)))

(defun format-sql-to-string (statement &rest args &key &allow-other-keys)
  (with-output-to-string (stream)
    (apply #'format-sql statement :stream stream args)))

(defgeneric format-sql-syntax-node (node database)
  (:documentation "Formats an SQL syntax node into *sql-stream*.")

  (:method ((s string) database)
           (write-string s *sql-stream*))

  (:method ((i integer) database)
           (write i :stream *sql-stream*)))

(defmethod execute-command :around (database transaction (command sql-statement) &optional visitor)
  (execute-command database transaction (format-sql-to-string command) visitor))
