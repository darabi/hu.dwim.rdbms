;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.rdbms.postgresql)

(def function execute-with-new-tx (&rest args)
  "If not executed with a new sub-transaction, some of the statements below (e.g. 'select * from pg_class') acquire an
   AccessShareLock on the table and subsequent 'create view' statements might hang, while waiting for the lock.
   To check for locks, execute *information-schema.show-locks* as a PostgreSQL user with appropriate privileges."
  (apply #'execute (append args '(:with-transaction :new))))

(def function list-objects (type)
  (map 'list [elt !1 0] (execute (format nil "SELECT relname FROM pg_class WHERE relkind = '~A'" type) :with-transaction :ensure)))

(def method database-list-sequences ((database postgresql))
  (list-objects "S"))

(def method database-list-tables ((database postgresql))
  (list-objects "r"))

(def method database-list-views ((database postgresql))
  (list-objects "v"))

(def method database-list-table-columns (name (database postgresql))
  (map 'list
       (lambda (column)
         (make-instance 'sql-column
                        :name (first-elt column)
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
                (string-downcase name))
        :with-transaction :ensure)))

(def method database-list-table-indices (name (database postgresql))
  (map 'list
       (lambda (row)
         (make-instance 'sql-index
                        :name (first-elt row)
                        :table-name name
                        :columns (database-list-index-columns (first-elt row) database)
                        :unique (elt row 1)))
       (execute
        (format nil "select distinct i.relname as index_name,
                            ix.indisunique
                       from pg_class t, pg_class i, pg_index ix, pg_attribute a
                       where t.oid = ix.indrelid
                         and i.oid = ix.indexrelid
                         and a.attrelid = t.oid
                         and a.attnum = ANY(ix.indkey)
                         and t.relkind = 'r'
                         and t.relname = '~A'
                       order by i.relname;"
                (string-downcase name))
        :with-transaction :ensure)))

(def method database-list-index-columns (name (database postgresql))
  (map 'list
       #'first-elt
       (execute
        (format nil "select a.attname as column_name
                       from pg_class t, pg_class i, pg_index ix, pg_attribute a
                       where t.oid = ix.indrelid
                         and i.oid = ix.indexrelid
                         and a.attrelid = t.oid
                         and a.attnum = ANY(ix.indkey)
                         and t.relkind = 'r'
                         and i.relname = '~A';"
                (string-downcase name))
        :with-transaction :ensure)))

(def method database-list-dependent-views (table column (database postgresql))
  (execute
   (format nil "select distinct c3.relname
                  from pg_attribute a
                       JOIN pg_class c ON (a.attrelid=c.oid)
                       JOIN pg_depend d ON (d.refobjid=c.oid AND d.refobjsubid=a.attnum)
                       JOIN pg_class c2 ON (d.classid=c2.oid AND c2.relname='pg_rewrite')
                       JOIN pg_rewrite r ON (d.objid=r.oid)
                       JOIN pg_class c3 ON (r.ev_class=c3.oid)
                  where c.relname='~A' and a.attname='~A' AND c3.relkind='v'"
           (string-downcase table) (string-downcase column))
   :with-transaction :ensure))

(def method database-list-table-primary-constraints (name (database postgresql))
  (remove-if-not (lambda (x) (search "pkey" x))
                 (list-table-indices name)
                 :key #'name-of))

(defun sql-rule-name-to-lisp (str)
  (let ((sym (find-symbol (string-upcase str) :keyword)))
    (case sym
      (:no\ action :defer-restrict)
      (:set\ null :set-null)
      (:set\ default :set-default)
      ((:cascade :restrict) sym)
      (t (error "invalid action: ~A" str)))))

;; PostgreSQL has a definition for constraint_column_usage which filters
;; data on ownership, causing problems when connecting as a different
;; user.  Here's the view definition without the problematic WHERE:
;;
(defparameter *information_schema.constraint_column_usage*
  "SELECT current_database()::information_schema.sql_identifier AS table_catalog,
       x.tblschema::information_schema.sql_identifier AS table_schema,
       x.tblname::information_schema.sql_identifier AS table_name,
       x.colname::information_schema.sql_identifier AS column_name,
       current_database()::information_schema.sql_identifier
         AS constraint_catalog,
       x.cstrschema::information_schema.sql_identifier AS constraint_schema,
       x.cstrname::information_schema.sql_identifier AS constraint_name
FROM(SELECT DISTINCT nr.nspname, r.relname, r.relowner, a.attname, nc.nspname,
       c.conname
     FROM pg_namespace nr,
          pg_class r,
          pg_attribute a,
          pg_depend d,
          pg_namespace nc,
          pg_constraint c
     WHERE nr.oid = r.relnamespace
       AND r.oid = a.attrelid
       AND d.refclassid = 'pg_class'::regclass::oid
       AND d.refobjid = r.oid
       AND d.refobjsubid = a.attnum
       AND d.classid = 'pg_constraint'::regclass::oid
       AND d.objid = c.oid
       AND c.connamespace = nc.oid
       AND c.contype = 'c'::\"char\"
       AND r.relkind = 'r'::\"char\"
       AND NOT a.attisdropped
   UNION ALL
     SELECT nr.nspname, r.relname, r.relowner, a.attname, nc.nspname, c.conname
     FROM pg_namespace nr,
          pg_class r,
          pg_attribute a,
          pg_namespace nc,
          pg_constraint c
     WHERE nr.oid = r.relnamespace
       AND r.oid = a.attrelid
       AND nc.oid = c.connamespace
       AND CASE
             WHEN c.contype = 'f'::\"char\" THEN
                  r.oid = c.confrelid AND (a.attnum = ANY (c.confkey))
             ELSE r.oid = c.conrelid AND (a.attnum = ANY (c.conkey))
             END
           AND NOT a.attisdropped
           AND (c.contype = ANY (ARRAY['p'::\"char\", 'u'::\"char\", 'f'::\"char\"]))
           AND r.relkind = 'r'::\"char\")
   x(tblschema, tblname, tblowner, colname, cstrschema, cstrname)")

(def method database-list-table-foreign-keys (table-name (database postgresql))
  (let ((catalog (getf (connection-specification-of *database*)
                       :database))
        (ccu (make-hash-table :test 'equal)))
    (map nil
         (lambda (row)
           (setf (gethash (elt row 0) ccu) row))
         (execute
          (format nil "SELECT constraint_name, table_name, column_name FROM (~A) ccu WHERE constraint_catalog='~A'"
                  *information_schema.constraint_column_usage*
                  catalog)
          :with-transaction :ensure))
    (map 'list
         (lambda (row)
           (let ((namtab (gethash (elt row 0) ccu)))
             (make-instance 'foreign-key-descriptor
                            :name (elt row 0)
                            :source-table (elt row 1)
                            :source-column (elt row 2)
                            :target-table (elt namtab 1)
                            :target-column (elt namtab 2)
                            :delete-rule (sql-rule-name-to-lisp (elt row 3))
                            :update-rule (sql-rule-name-to-lisp (elt row 4)))))
         (execute
          (format nil "SELECT DISTINCT
                  tc.constraint_name,
                  tc.table_name,
                  kcu.column_name,
                  rc.delete_rule,
                  rc.update_rule
                FROM
                  information_schema.table_constraints AS tc
                  JOIN information_schema.key_column_usage AS kcu
                    ON tc.constraint_name = kcu.constraint_name
                    AND tc.constraint_catalog = kcu.constraint_catalog
                  JOIN information_schema.referential_constraints AS rc
                    ON rc.constraint_name = tc.constraint_name
                    AND rc.constraint_catalog = tc.constraint_catalog
                WHERE constraint_type = 'FOREIGN KEY'
                  AND tc.constraint_catalog='~A'
                  AND tc.table_name='~A';"
                  catalog
                  (string-downcase table-name))
          :with-transaction :ensure))))

(def method database-list-view-definitions ((database postgresql))
  (execute "select viewname, definition from pg_views"
           :result-type 'list
           :with-transaction :ensure))

(def method database-view-definition (view-name (database postgresql))
  (caar (execute (format nil "select definition from pg_views where viewname='~A'"
                         view-name)
                 :result-type 'list
                 :with-transaction :ensure)))

(def method database-list-table-check-constraints (name (database postgresql))
  (map 'list
       (lambda (row) (elt row 0))
       (execute
        (format nil "SELECT constraint_name
                FROM information_schema.table_constraints
                WHERE constraint_type = 'CHECK' AND table_name = '~a'"
                (string-downcase name))
        :result-type 'list
        :with-transaction :ensure)))

;;
;; Useful query for showing which pgsql process is waiting on which lock
;;
(defparameter *information-schema.show-locks* "
SELECT blocked_locks.pid         AS blocked_pid,
       blocked_activity.usename  AS blocked_user,
       blocked_locks.locktype    AS blocked_type,
       blocked_locks.mode        AS blocked_mode,
       blocking_locks.pid        AS blocking_pid,
       blocking_activity.usename AS blocking_user,
       blocking_locks.locktype   AS blocking_type,
       blocking_locks.mode       AS blocking_mode,
       blocked_activity.query    AS blocked_statement,
       blocking_activity.query   AS current_statement_in_blocking_process
FROM pg_catalog.pg_locks         blocked_locks
    JOIN pg_catalog.pg_stat_activity blocked_activity  ON blocked_activity.pid = blocked_locks.pid
    JOIN pg_catalog.pg_locks         blocking_locks
        ON blocking_locks.locktype = blocked_locks.locktype
        AND blocking_locks.DATABASE IS NOT DISTINCT FROM blocked_locks.DATABASE
        AND blocking_locks.relation IS NOT DISTINCT FROM blocked_locks.relation
        AND blocking_locks.page IS NOT DISTINCT FROM blocked_locks.page
        AND blocking_locks.tuple IS NOT DISTINCT FROM blocked_locks.tuple
        AND blocking_locks.virtualxid IS NOT DISTINCT FROM blocked_locks.virtualxid
        AND blocking_locks.transactionid IS NOT DISTINCT FROM blocked_locks.transactionid
        AND blocking_locks.classid IS NOT DISTINCT FROM blocked_locks.classid
        AND blocking_locks.objid IS NOT DISTINCT FROM blocked_locks.objid
        AND blocking_locks.objsubid IS NOT DISTINCT FROM blocked_locks.objsubid
        AND blocking_locks.pid != blocked_locks.pid
    JOIN pg_catalog.pg_stat_activity blocking_activity ON blocking_activity.pid = blocking_locks.pid
   WHERE NOT blocked_locks.GRANTED;
")
