;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms)

(defun sql-literal-values-for (columns values)
  (map 'list (lambda (column value)
               (if (or (typep value 'sql-literal)
                       (not (typep column 'sql-column)))
                   value
                   (make-instance 'sql-literal
                                  :value value
                                  :type (type-of column))))
       columns values))

(defun insert-record (table columns values)
  (nth-value 1 (execute (make-instance 'sql-insert
                                       :table table
                                       :columns columns
                                       :values (sql-literal-values-for columns values)))))

(defun update-records (table columns values &optional where)
  (nth-value 1 (execute (make-instance 'sql-update
                                       :table table
                                       :columns columns
                                       :values (sql-literal-values-for columns values)
                                       :where where))))

(defun delete-records (table &optional where)
  (nth-value 1 (execute (make-instance 'sql-delete
                                       :table table
                                       :where where))))

(defun select-records (columns tables &key where group-by having order-by offset limit)
  (execute (make-instance 'sql-select
                          :columns columns
                          :tables tables
                          :where where
                          :group-by group-by
                          :having having
                          :order-by order-by
                          :offset offset
                          :limit limit)))

(defun select-count-* (tables &optional where)
  (first* (first* (execute (make-instance 'sql-select
                                          :columns (list (sql-count-*))
                                          :tables tables
                                          :where where)))))
