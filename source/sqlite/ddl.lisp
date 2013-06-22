;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.rdbms.sqlite)

(def method database-list-sequences ((database sqlite))
  (error "Not implemented"))

(def method database-list-tables ((database sqlite))
  (map 'list [elt !1 0]
       (execute "SELECT name FROM sqlite_master WHERE type = 'table'")))

(def method database-list-table-columns (name (database sqlite))
  (error "Not implemented"))

(def method database-list-table-indices (name (database sqlite))
  (map 'list
       (lambda (column)
         (make-instance 'sql-index
                        :name (elt column 1)
                        :columns (database-list-index-columns (elt column 1) database)
                        :table-name name
                        :unique (eql 1 (elt column 2))))
       (execute
        (format nil "PRAGMA index_list('~A')"
                (string-downcase name)))))

(def method database-list-index-columns (name (database sqlite))
  (map 'list (lambda (row) (elt row 2))
       (execute
        (format nil "PRAGMA index_info('~A')"
                (string-downcase name)))))
