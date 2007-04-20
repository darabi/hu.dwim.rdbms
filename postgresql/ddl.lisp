;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms.postgresql)

#.(file-header)

(defun list-objects (type)
  (map 'list #L(elt !1 0) (execute (format nil "SELECT relname FROM pg_class WHERE relkind = '~A'" type))))

(defmethod database-list-sequences ((database postgresql))
  (list-objects "S"))

(defmethod database-list-tables ((database postgresql))
  (list-objects "r"))

(defmethod database-list-table-columns (name (database postgresql))
  (map 'list
   (lambda (column)
     (make-instance 'sql-column
                    :name (first* column)
                    :type (sql-type-for-internal-type (subseq column 1))))
   (execute
    (format nil "SELECT pg_attribute.attname, pg_type.typname, pg_attribute.attlen,
                                   pg_attribute.atttypmod, pg_attribute.attnotnull
                            FROM pg_type, pg_class, pg_attribute
                            WHERE pg_class.oid = pg_attribute.attrelid AND
                                  pg_class.relname = '~A' AND
                                  pg_attribute.attname NOT IN ('cmin', 'cmax', 'xmax', 'xmin', 'oid', 'ctid', 'tableoid') AND
                                  pg_attribute.attisdropped = FALSE AND
                                  pg_attribute.atttypid = pg_type.oid"
            (string-downcase name)))))

(defmethod database-list-table-indices (table-name (database postgresql))
  (mapcar
   (lambda (column)
     (make-instance 'sql-index
                    :name (first column)
                    :table-name table-name))
   (execute
    (format nil "select indexname from pg_indexes where tablename = '~A'"
            (string-downcase table-name)))))
