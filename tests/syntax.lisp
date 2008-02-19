;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms-test)

#.(cl-rdbms::file-header)

(enable-sql-syntax)

(def suite (syntax :in-suite 'test))

(in-suite syntax)

(def definer syntax-test* (name sexp-p &body body)
  `(def test ,name ()
     (with-database *test-database*
       ,@(iter (for (sql string-or-case-body) :on body :by #'cddr)
               (collect `(is (equalp
                              (format-sql-to-string ,(if sexp-p
                                                         `(compile-sexp-sql ,sql)
                                                         sql))
                              ,(if (stringp string-or-case-body)
                                   string-or-case-body
                                   `(typecase *database*
                                      ,@string-or-case-body)))))))))

(def definer syntax-test (name &body body)
  `(def syntax-test* ,name #t ,@body))

(def definer ast-test (name &body body)
  `(def syntax-test* ,name #f ,@body))

(def ast-test test/identifier-syntax
  (make-instance 'sql-identifier :name "a")
  "a"

  (make-instance 'sql-identifier :name 'a)
  "a")

(def ast-test test/create-table-syntax
  (make-instance 'sql-create-table
                 :name "a"
                 :columns (list (make-instance 'sql-column :name "a" :type (make-instance 'sql-integer-type))))
  ((oracle "CREATE TABLE a (a NUMBER)")
   (t "CREATE TABLE a (a NUMERIC)"))

  (make-instance 'sql-create-table
                 :temporary :drop
                 :name "a"
                 :columns (list (make-instance 'sql-column :name "a" :type (make-instance 'sql-integer-type))))
  ((oracle "CREATE GLOBAL TEMPORARY TABLE a (a NUMBER) ON COMMIT DROP")
   (t "CREATE GLOBAL TEMPORARY TABLE a (a NUMERIC) ON COMMIT DROP")))

(def ast-test test/alter-table-syntax
  (make-instance 'sql-alter-table
                 :name "a"
                 :actions (list (make-instance 'sql-add-column-action :name "a" :type (make-instance 'sql-integer-type))))
  ((oracle "ALTER TABLE a ADD (a NUMBER)")
   (t "ALTER TABLE a ADD a NUMERIC"))

  (make-instance 'sql-alter-table
                 :name "a"
                 :actions (list (make-instance 'sql-alter-column-type-action :name "a" :type (make-instance 'sql-integer-type))))
  ((oracle "ALTER TABLE a ALTER COLUMN a TYPE NUMBER")
   (t "ALTER TABLE a ALTER COLUMN a TYPE NUMERIC"))

  (make-instance 'sql-alter-table
                 :name "a"
                 :actions (list (make-instance 'sql-drop-column-action :name "a")))
  "ALTER TABLE a DROP COLUMN a")

(def ast-test test/drop-table-syntax
  (make-instance 'sql-drop-table
                 :name "a")
  "DROP TABLE a")

(def ast-test test/create-index-syntax
  (make-instance 'sql-create-index
                 :name "a"
                 :table-name "a"
                 :columns (list "a" "a"))
  "CREATE INDEX a ON a (a, a)")

(def ast-test test/drop-index-syntax
  (make-instance 'sql-drop-index
                 :name "a")
  "DROP INDEX a")

(def ast-test test/insert-syntax
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

(def ast-test test/select-syntax
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
  "SELECT a FROM a WHERE (a = b)"

  (make-instance 'sql-select
                 :columns (list (make-instance 'sql-identifier :name "a"))
                 :tables (list (make-instance 'sql-identifier :name "a")))
  "SELECT a FROM a"

  (make-instance 'sql-select
                 :columns (list (make-instance 'sql-column-alias :column "a" :table "b" :alias "c"))
                 :tables (list (make-instance 'sql-table-alias :name "a" :alias "b")))
  "SELECT b.a AS c FROM a b")

(def ast-test test/update-syntax
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

(def ast-test test/delete-syntax
  (make-instance 'sql-delete
                 :table "a")
  "DELETE from a"

  (make-instance 'sql-delete
                 :table (make-instance 'sql-identifier :name "a"))
  "DELETE from a")

(def ast-test test/sequence-syntax
  (make-instance 'sql-create-sequence
                 :name "a")
  "CREATE SEQUENCE a"

  (make-instance 'sql-drop-sequence
                 :name "a")
  "DROP SEQUENCE a"

  (make-instance 'sql-select
                 :columns (list (make-instance 'sql-sequence-nextval-column :name "a")))
  ((oracle "SELECT a.nextval FROM dual ")
   (t "SELECT NEXTVAL('a')")))

(def syntax-test test/sexp-syntax
  '(select "bar" table)
  ((oracle "SELECT bar FROM \"table\"")
   (t "SELECT bar FROM table"))

  '(select (count *) _table)
  ((oracle "SELECT count(*) FROM \"_table\"")
   (t "SELECT count(*) FROM _table"))

  '(select
    ((foo.col1 "col1_alias") "bar")
    table)
  ((oracle "SELECT foo.col1 AS col1_alias, bar FROM \"table\"")
   (t "SELECT foo.col1 AS col1_alias, bar FROM table"))

  `(select
    (foo.column "bar")
    ,(list (make-instance 'sql-table-alias
                          :name "alma"
                          :alias "alma_alias")))
  ((oracle "SELECT foo.\"column\", bar FROM alma alma_alias")
   (t "SELECT foo.column, bar FROM alma alma_alias"))

  '(create table (:temporary :drop) alma ((col1 varchar) ("col2" (integer 32))))
  ((oracle "CREATE GLOBAL TEMPORARY TABLE alma (col1 VARCHAR2, col2 NUMBER(10)) ON COMMIT DROP")
   (t "CREATE GLOBAL TEMPORARY TABLE alma (col1 CHARACTER VARYING, col2 INT) ON COMMIT DROP"))

  '(create table (:temporary :delete-rows) alma (("col2" (integer 32))))
  ((oracle "CREATE GLOBAL TEMPORARY TABLE alma (col2 NUMBER(10)) ON COMMIT DELETE ROWS")
   (t "CREATE GLOBAL TEMPORARY TABLE alma (col2 INT) ON COMMIT DELETE ROWS")))
