;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms.sqlite)

(defmethod database-list-sequences ((database sqlite))
  (error "Not implemented"))

(defmethod database-list-tables ((database sqlite))
  (map 'list #L(elt !1 0)
       (execute "SELECT name FROM sqlite_master WHERE type = 'table'")))

(defmethod database-list-table-columns (name (database sqlite))
  (error "Not implemented"))

(defmethod database-list-table-indices (name (database sqlite))
  (map 'list
       (lambda (column)
         (make-instance 'sql-index
                        :name (first* column)
                        :table-name name))
       (execute
        (format nil "SELECT name FROM sqlite_master WHERE type = 'index' AND tbl_name = '~A'"
                (string-downcase name)))))
