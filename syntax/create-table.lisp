;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms)

#.(file-header)

(defclass* sql-create-table (sql-statement)
  ((table-name
    :type symbol))
  (:documentation "An SQL CREATE TABLE statement."))
