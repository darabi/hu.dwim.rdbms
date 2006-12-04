;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms)

#.(file-header)

(defun insert-records (table columns values)
  (execute (make-instance 'sql-insert
                          :table table
                          :columns columns
                          :values values)))

(defun update-records (table columns values where)
  (execute (make-instance 'sql-update
                          :table table
                          :columns columns
                          :values values
                          :where where)))

(defun delete-records (table &optional where)
  (execute (make-instance 'sql-delete
                          :table table
                          :where where)))

(defun select-records (columns tables where)
  (execute (make-instance 'sql-select
                          :columns columns
                          :tables tables
                          :where where)))
