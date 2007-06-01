;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms.oracle)

#.(file-header)

(defmethod database-list-sequences ((database oracle))
  (mapcar #'first (execute "select sequence_name from user_sequences")))

(defmethod database-list-tables ((database oracle))
  (mapcar #'first (execute "select table_name from user_tables")))

(defmethod database-list-table-columns (name (database oracle))
  (mapcar
   (lambda (column)
     (make-instance 'sql-column
                    :name (first* column)
                    :type (sql-type-for-internal-type (subseq column 1))))
   (execute
    (format nil "select column_name, data_type, data_length, data_scale, nullable from user_tab_columns where table_name = '~A'"
            (string-downcase name)))))

(defmethod database-list-table-indices (name (database oracle))
  (mapcar
   (lambda (column)
     (make-instance 'sql-index
                    :name (first column)
                    :table-name name))
   (execute
    (format nil "select index_name from user_indexes where table_name = '~A'"
            (string-downcase name)))))
