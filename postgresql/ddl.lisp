;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms)

#.(file-header)

(defun list-objects (type)
  (mapcar #'car (execute (format nil "SELECT relname FROM pg_class WHERE relkind = '~A'" type))))

(defmethod database-list-tables ((database postgresql))
  (list-objects "r"))

(defmethod database-list-table-columns (name (database postgresql))
  (mapcar
   (lambda (column)
     (make-instance 'sql-column
                    :name (first column)
                    :type (sql-column-type-for-internal-type (rest column))))
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
