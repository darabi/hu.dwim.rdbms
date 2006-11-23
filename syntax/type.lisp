;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms)

#.(file-header)

(defclass* sql-type (sql-syntax-node)
  ()
  (:documentation "Base class for all SQL types."))

(defclass* sql-integer-16-type (sql-type)
  ())

(defclass* sql-integer-32-type (sql-type)
  ())

(defclass* sql-integer-64-type (sql-type)
  ())
