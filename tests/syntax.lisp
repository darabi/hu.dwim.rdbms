;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms-test)

#.(cl-rdbms::file-header)

(enable-sql-syntax)

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


(def suite (syntax :in-suite 'test))
(in-suite syntax)

(def syntax-test test/syntax/sexp
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

(def test test/syntax/expand-sql-ast/1 (&optional (n 3))
  (bind ((expected (format nil "SELECT a, b FROM t WHERE (~A)"
                           (apply 'concatenate 'string
                                  (iter (for i :from 1 :to n)
                                        (unless (first-iteration-p)
                                          (collect " OR "))
                                        (collect "t.b"))))))
    ;; using the reader
    (is (string=
         expected
         (funcall
          (compile
           nil
           [select (a b) t ,(apply 'sql-or
                                   (iter (repeat n)
                                         (collect (sql-column-alias :table 't :column 'b))))]))))
    ;; building the AST by hand (the builder expands to something very similar to this)
    (is (string=
         expected
         (funcall
          (compile
           nil
           (expand-sql-ast-into-lambda-form
            (sql-select :columns '(a b)
                        :tables '(t)
                        :where (sql-unquote
                                 :form
                                 `(apply 'sql-or
                                         (iter (repeat ,n)
                                               (collect ,(sql-column-alias :table 't :column 'b)))))))))))))

(def test test/syntax/expand-sql-ast/2 (&optional (n 3))
  (bind ((expected (format nil "SELECT a, b FROM t WHERE (~A)"
                           (apply 'concatenate 'string
                                  (iter (for i :from 1 :to n)
                                        (unless (first-iteration-p)
                                          (collect " OR "))
                                        (collect (format nil "(a = (b + $~d::NUMERIC + ~d))" i i)))))))
    (is (string=
         expected
         (funcall
          (compile
           nil
           [select (a b) t ,(apply 'sql-or
                                   (iter (for i :from 1 :to n)
                                         (rebind (i)
                                           (collect (sql-= (sql-identifier :name 'a)
                                                           (sql-+ (sql-identifier :name 'b)
                                                                  (sql-binding-variable
                                                                     :type (sql-integer-type)
                                                                     :name i)
                                                                  (sql-literal :value i)))))))]))))
    (is (string=
         expected
         (funcall
          (compile
           nil
           (expand-sql-ast-into-lambda-form
            (sql-select :columns '(a b)
                        :tables '(t)
                        :where (sql-unquote
                                 :form
                                 `(apply 'sql-or
                                         (iter (for i :from 1 :to ,n)
                                               (rebind (i)
                                                 (collect ,(sql-= (sql-identifier :name 'a)
                                                                  (sql-+ (sql-identifier :name 'b)
                                                                         (sql-unquote :form '(sql-binding-variable
                                                                                              :type (sql-integer-type)
                                                                                              :name i))
                                                                         (sql-unquote :form '(sql-literal :value i)))))))))))))))))


(def suite (formatting :in-suite 'syntax))
(in-suite formatting)

(def ast-test test/syntax/formatting/identifier
  (sql-identifier :name "alma")
  "alma"

  (sql-identifier :name 'alma)
  "alma")

(def ast-test test/syntax/formatting/create-table
  (sql-create-table :name "a"
                    :columns (list (sql-column :name "a"
                                               :type (sql-integer-type))))
  ((oracle "CREATE TABLE a (a NUMBER)")
   (t "CREATE TABLE a (a NUMERIC)"))

  (sql-create-table :temporary :drop
                    :name "a"
                    :columns (list (sql-column :name "a"
                                               :type (sql-integer-type))))
  ((oracle "CREATE GLOBAL TEMPORARY TABLE a (a NUMBER) ON COMMIT DROP")
   (t "CREATE GLOBAL TEMPORARY TABLE a (a NUMERIC) ON COMMIT DROP")))

(def ast-test test/syntax/formatting/alter-table
  (sql-alter-table :name "a"
                   :actions (list (sql-add-column-action :name "a"
                                                         :type (sql-integer-type))))
  ((oracle "ALTER TABLE a ADD (a NUMBER)")
   (t "ALTER TABLE a ADD a NUMERIC"))

  (sql-alter-table :name "a"
                   :actions (list (sql-alter-column-type-action :name "a"
                                                                :type (sql-integer-type))))
  ((oracle "ALTER TABLE a ALTER COLUMN a TYPE NUMBER")
   (t "ALTER TABLE a ALTER COLUMN a TYPE NUMERIC"))

  (sql-alter-table :name "a"
                   :actions (list (sql-drop-column-action :name "a")))
  "ALTER TABLE a DROP COLUMN a")

(def ast-test test/syntax/formatting/drop-table
  (sql-drop-table :name "a")
  "DROP TABLE a")

(def ast-test test/syntax/formatting/create-index
  (sql-create-index :name "a"
                    :table-name "a"
                    :columns (list "a" "a"))
  "CREATE INDEX a ON a (a, a)")

(def ast-test test/syntax/formatting/drop-index
  (sql-drop-index :name "a")
  "DROP INDEX a")

(def ast-test test/syntax/formatting/insert
  (sql-insert :table "a"
              :columns (list "a")
              :values (list "a"))
  "INSERT INTO a (a) VALUES ('a')"

  (sql-insert :table (sql-identifier :name "a")
              :columns (list (sql-identifier :name "a"))
              :values (list "a"))
  "INSERT INTO a (a) VALUES ('a')")

(def ast-test test/syntax/formatting/select
  (sql-select :columns (list "a")
              :tables (list "a"))
  "SELECT a FROM a"

  (sql-select :columns (list (sql-all-columns))
              :tables (list "a"))
  "SELECT * FROM a"

  (sql-select :columns (list "a")
              :tables (list "a")
              :where (sql-binary-operator :name '=
                                          :left (sql-identifier :name "a")
                                          :right (sql-identifier :name "b")))
  "SELECT a FROM a WHERE (a = b)"

  (sql-select :columns (list (sql-identifier :name "a"))
              :tables (list (sql-identifier :name "a")))
  "SELECT a FROM a"

  (sql-select :columns (list (sql-column-alias :column "a" :table "b" :alias "c"))
              :tables (list (sql-table-alias :name "a" :alias "b")))
  "SELECT b.a AS c FROM a b")

(def ast-test test/syntax/formatting/update
  (sql-update :table "a"
              :columns (list "a")
              :values (list "a"))
  "UPDATE a SET a = 'a'"

  (sql-update :table (sql-identifier :name "a")
              :columns (list (sql-identifier :name "a"))
              :values (list "a"))
  "UPDATE a SET a = 'a'")

(def ast-test test/syntax/formatting/delete
  (sql-delete :table "a")
  "DELETE from a"

  (sql-delete :table (make-instance 'sql-identifier :name "a"))
  "DELETE from a")

(def ast-test test/syntax/formatting/sequence
  (sql-create-sequence :name "a")
  "CREATE SEQUENCE a"

  (sql-drop-sequence :name "a")
  "DROP SEQUENCE a"

  (sql-select :columns (list (sql-sequence-nextval-column :name "a")))
  ((oracle "SELECT a.nextval FROM dual ")
   (t "SELECT NEXTVAL('a')")))

