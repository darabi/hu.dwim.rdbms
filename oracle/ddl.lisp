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
  (map 'list
   (lambda (column)
     (make-instance 'sql-column
                    :name (svref column 0)
                    :type (sql-type-for-internal-type
                           (svref column 1)
                           (svref column 2)
                           (svref column 3)
                           (svref column 4))))
   (execute
    (format nil "select column_name, data_type, data_length, data_precision, data_scale from user_tab_columns where table_name = '~A'"
            name)
    :result-type 'vector)))

(defmethod database-list-table-indices (name (database oracle))
  (mapcar
   (lambda (column)
     (make-instance 'sql-index
                    :name (first column)
                    :table-name name))
   (execute
    (format nil "select index_name from user_indexes where table_name = '~A'"
            (string-downcase name)))))
