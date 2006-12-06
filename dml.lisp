;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms)

#.(file-header)

(defun sql-literal-values-for (columns values)
  (mapcar (lambda (column value)
            (if (typep value 'sql-literal)
                value
                (make-instance 'sql-literal
                               :value value
                               :type (type-of column))))
          columns values))

(defun insert-records (table columns values)
  (execute(make-instance 'sql-insert
                         :table table
                         :columns columns
                         :values (sql-literal-values-for columns values))))

(defun update-records (table columns values &optional where)
  (execute (make-instance 'sql-update
                          :table table
                          :columns columns
                          :values (sql-literal-values-for columns values)
                          :where where)))

(defun delete-records (table &optional where)
  (execute (make-instance 'sql-delete
                          :table table
                          :where where)))

(defun select-records (columns tables &optional where)
  (execute (make-instance 'sql-select
                          :columns columns
                          :tables tables
                          :where where)))
