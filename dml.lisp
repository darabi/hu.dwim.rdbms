;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms)

#.(file-header)

(defun insert-records (table-name columns values)
  (execute (make-instance 'sql-insert
                          :table-name table-name
                          :columns columns
                          :values values)))

(defun update-records (table-name columns values where)
  (execute (make-instance 'sql-update
                          :table-name table-name
                          :columns columns
                          :values values
                          :where where)))

(defun delete-records (table-name where)
  (execute (make-instance 'sql-delete
                          :table-name table-name
                          :where where)))

(defun select-records (table-aliases column-aliases where)
  (execute (make-instance 'sql-select
                          :table-aliases table-aliases
                          :column-aliases column-aliases
                          :where where)))
