;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms-test)

#.(cl-rdbms::file-header)

(eval-always
  (import (let ((*package* (find-package :cl-rdbms)))
            (read-from-string "(enable-sharp-boolean-syntax
                                connection-specification-of *database* *transaction*
                                with-transaction*
                                log log.dribble log.debug log.info log.warn log.error)")))
  (import-sql-syntax-node-names))

(def-suite :cl-rdbms :description "cl-rdbms tests")

(in-suite :cl-rdbms)

(defparameter *test-database* (make-instance 'postgresql-pg  :connection-specification
                                             '(:database "dwim" :user-name "root" :password "admin123")))

(defmacro with-test-transaction (&body body)
  `(with-transaction* (:database *test-database*)
    ,@body))

(defmacro test* (name &body body)
  `(test (,name :depends-on connect)
    ,@body))

(test connect
  (finishes
    (with-test-transaction
      (execute "select now()"))))

(test* create-table
  (finishes
    (with-test-transaction
      (unwind-protect
           (execute "CREATE TABLE alma ()")
        (ignore-errors
          (execute "DROP TABLE alma"))))))

(test* encoding
  (finishes
    (with-test-transaction
      (unwind-protect
           (progn
             (execute "CREATE TABLE alma (
                       name varchar(40))")
             (execute "INSERT INTO alma VALUES ('éáúóüőű')")
             (execute "SELECT NAME FROM alma"))
        (ignore-errors
          (execute "DROP TABLE alma"))))))

(defmacro syntax-test (name sexp-p &body body)
  `(test (,name)
    (with-database *test-database*
      ,@ (loop for (sql string) :on body :by #'cddr
               collect `(is (equalp
                             (format-sql-to-string ,(if sexp-p
                                                        `(sql ,sql)
                                                        sql))
                             ,string))))))

(syntax-test identifier-syntax #f
  (make-instance 'sql-identifier :name "a")
  "a"

  (make-instance 'sql-identifier :name 'a)
  "a")

(syntax-test create-table-syntax #f
  (make-instance 'sql-create-table
                 :name "a"
                 :columns (list (make-instance 'sql-column :name "a" :type (make-instance 'sql-integer-type))))
  "CREATE TABLE a (a NUMERIC)")

(syntax-test alter-table-syntax #f
  (make-instance 'sql-alter-table
                 :name "a"
                 :actions (list (make-instance 'sql-add-column-action :name "a" :type (make-instance 'sql-integer-type))))
  "ALTER TABLE a ADD a NUMERIC"

  (make-instance 'sql-alter-table
                 :name "a"
                 :actions (list (make-instance 'sql-alter-column-type-action :name "a" :type (make-instance 'sql-integer-type))))
  "ALTER TABLE a ALTER COLUMN a TYPE NUMERIC"

  (make-instance 'sql-alter-table
                 :name "a"
                 :actions (list (make-instance 'sql-drop-column-action :name "a")))
  "ALTER TABLE a DROP COLUMN a")

(syntax-test drop-table-syntax #f
  (make-instance 'sql-drop-table
                 :name "a")
  "DROP TABLE a")

(syntax-test insert-syntax #f
  (make-instance 'sql-insert
                 :table "a"
                 :columns (list "a")
                 :values (list "a"))
  "INSERT INTO a (a) VALUES ('a')"
  
  (make-instance 'sql-insert
                 :table (make-instance 'sql-identifier :name "a")
                 :columns (list (make-instance 'sql-identifier :name "a"))
                 :values (list "a"))
  "INSERT INTO a (a) VALUES ('a')")

(syntax-test select-syntax #f
  (make-instance 'sql-select
                 :columns (list "a")
                 :tables (list "a"))
  "SELECT a FROM a"
  
  (make-instance 'sql-select
                 :columns (list (make-instance 'sql-all-columns))
                 :tables (list "a"))
  "SELECT * FROM a"
  
  (make-instance 'sql-select
                 :columns (list "a")
                 :tables (list "a")
                 :where (make-instance 'sql-binary-operator
                                       :name '=
                                       :left (make-instance 'sql-identifier :name "a")
                                       :right (make-instance 'sql-identifier :name "b")))
  "SELECT a FROM a WHERE a = b"
  
  (make-instance 'sql-select
                 :columns (list (make-instance 'sql-identifier :name "a"))
                 :tables (list (make-instance 'sql-identifier :name "a")))
  "SELECT a FROM a"
  
  (make-instance 'sql-select
                 :columns (list (make-instance 'sql-column-alias :column "a" :table "b" :alias "c"))
                 :tables (list (make-instance 'sql-table-alias :name "a" :alias "b")))
  "SELECT b.a AS c FROM a b")
  
(syntax-test update-syntax #f
  (make-instance 'sql-update
                 :table "a"
                 :columns (list "a")
                 :values (list "a"))
  "UPDATE a SET a = 'a'"
  
  (make-instance 'sql-update
                 :table (make-instance 'sql-identifier :name "a")
                 :columns (list (make-instance 'sql-identifier :name "a"))
                 :values (list "a"))
  "UPDATE a SET a = 'a'")
  
(syntax-test delete-syntax #f
  (make-instance 'sql-delete
                 :table "a")
  "DELETE from a"

  (make-instance 'sql-delete
                 :table (make-instance 'sql-identifier :name "a"))
  "DELETE from a")

(syntax-test sexp-syntax #t
  `(select "bar" table)
  "SELECT bar FROM table"

  `(select (count *) table)
  "SELECT count(*) FROM table"

  `(select
    ((foo.col1 "col1_alias") "bar")
    table)
  "SELECT foo.col1 AS col1_alias, bar FROM table"

  `(select
    (foo.column "bar")
    ,(progn
      (list (make-instance 'sql-table-alias
                           :name "alma"
                           :alias "alma_alias"))))
  "SELECT foo.column, bar FROM alma alma_alias")
