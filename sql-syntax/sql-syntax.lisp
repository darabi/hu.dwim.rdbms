;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms)

#.(file-header)

(defvar *sql-stream*)

(defclass* sql-expression ()
  ())

(defun format-sql (expression &key (stream t) (database *database*))
  (let ((*sql-stream* stream)
        (*database* database))
    (format-sql-expression expression database)))

(defun format-sql-to-string (expression &rest args &key &allow-other-keys)
  (with-output-to-string (stream)
    (apply #'format-sql expression :stream stream args)))

(defgeneric format-sql-expression (expression database))
