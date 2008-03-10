;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms-test)

#.(cl-rdbms::file-header)

(enable-sql-syntax)

(def definer syntax-test (name database args &body body)
  `(def test ,name ,args
     (with-database (closer-mop:class-prototype
                     (find-class
                      ',(if (eq database t)
                            'database
                            database)))
       ,@body)))

(def definer dialect-test (name kind &body body)
  `(def test ,name ()
     (with-database *test-database*
       ,@(iter (for (sql string-or-case-body) :on body :by #'cddr)
               (collect `(is (equalp
                              ,(ecase kind
                                 (:sexp   `(format-sql-to-string (compile-sexp-sql ,sql)))
                                 (:ast    `(format-sql-to-string ,sql))
                                 (:reader (rebinding (sql)
                                            `(etypecase ,sql
                                               (string ,sql)
                                               (function (funcall ,sql))))))
                              ,(if (stringp string-or-case-body)
                                   string-or-case-body
                                   `(typecase *database*
                                      ,@string-or-case-body)))))))))

(def definer sexp-sql-dialect-test (name &body body)
  `(def dialect-test ,name :sexp ,@body))

(def definer ast-dialect-test (name &body body)
  `(def dialect-test ,name :ast ,@body))

(def definer reader-dialect-test (name &body body)
  `(def dialect-test ,name :reader ,@body))


(def suite (syntax :in-suite 'test))
(in-suite syntax)

(def sexp-sql-dialect-test test/syntax/sexp-dialect
  '(select "bar" table)
  ((oracle "SELECT \"bar\" FROM \"table\"")
   (t "SELECT bar FROM table"))

  '(select (count *) _table)
  ((oracle "SELECT count(*) FROM \"_table\"")
   (t "SELECT count(*) FROM _table"))

  '(select ((foo.col1 "col1_alias") "bar") table)
  ((oracle "SELECT \"foo\".\"col1\" AS \"col1_alias\", \"bar\" FROM \"table\"")
   (t "SELECT foo.col1 AS col1_alias, bar FROM table"))

  `(select
    (foo.column "bar")
    ,(list (sql-table-alias :name "alma" :alias "alma_alias")))
  ((oracle "SELECT \"foo\".\"column\", \"bar\" FROM \"alma\" \"alma_alias\"")
   (t "SELECT foo.column, bar FROM alma alma_alias"))

  '(create table (:temporary :drop) alma ((col1 varchar) ("col2" (integer 32))))
  ((oracle "CREATE GLOBAL TEMPORARY TABLE \"alma\" (\"col1\" VARCHAR2, \"col2\" NUMBER(10)) ON COMMIT DROP")
   (t "CREATE GLOBAL TEMPORARY TABLE alma (col1 CHARACTER VARYING, col2 INT) ON COMMIT DROP"))

  '(create table (:temporary :delete-rows) alma (("col2" (integer 32))))
  ((oracle "CREATE GLOBAL TEMPORARY TABLE \"alma\" (\"col2\" NUMBER(10)) ON COMMIT DELETE ROWS")
   (t "CREATE GLOBAL TEMPORARY TABLE alma (col2 INT) ON COMMIT DELETE ROWS")))

(def reader-dialect-test test/syntax/sql-reader
  [select "bar" table]
  "SELECT bar FROM table"

  (bind ((column-1 (sql-column-alias :table t :column 'col1 :alias "foo"))
         (column-2 (sql-column-alias :table t :column 'col2 :alias "bar")))
    [select (,column-1 ,column-2) table])
  "SELECT t.col1 AS foo, t.col2 AS bar FROM table"

  (bind ((columns (list (sql-column :name 'col1 :type (sql-integer-type))
                        (sql-column :name 'col2 :type (sql-character-varying-type))
                        (sql-column :name 'col3 :type (sql-character-varying-type))
                        (sql-column :name 'col4 :type (sql-float-type)))))
    [insert t ,columns (42
                        ,(sql-literal :value (string-upcase "some random text")
                                      :type (sql-character-varying-type))
                        ,(sql-binding-variable :name 'dynamic-binding
                                               :type (sql-character-varying-type))
                        (? 'static-binding (float 32)))])
  ((postgresql "INSERT INTO t (col1, col2, col3, col4) VALUES (42, $2::CHARACTER VARYING, $3::CHARACTER VARYING, $1::FLOAT4)")))

(def syntax-test test/syntax/expand-sql-ast/unquote/1 postgresql (&optional (n 3))
  ;; "SELECT a, b FROM t WHERE (t.b OR t.b OR t.b)"
  (bind ((expected (format nil "SELECT a, b FROM t WHERE (~A)"
                           (apply 'concatenate 'string
                                  (iter (for i :from 1 :to n)
                                        (unless (first-iteration-p)
                                          (collect " OR "))
                                        (collect "t.b"))))))
    ;; advanced use of the reader: the criteria is generated into a variable,
    ;; it could even be the input of the function.
    (bind ((criteria [or ,@(iter (repeat (1- n))
                                 (collect (sql-column-alias :table 't :column 'b)))
                         ,(sql-column-alias :table 't :column 'b)]))
      (is (string= expected
                   (funcall [select (a b) t ,criteria]))))
    ;; building the AST by hand
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

(def syntax-test test/syntax/expand-sql-ast/unquote/2 postgresql (&optional (n 3))
  ;; "SELECT a, b FROM t WHERE ((a = (b + $1::NUMERIC + 1)) OR (a = (b + $2::NUMERIC + 2)) OR (a = (b + $3::NUMERIC + 3)))"
  (bind ((expected (format nil "SELECT a, b FROM t WHERE (~A)"
                           (apply 'concatenate 'string
                                  (iter (for i :from 1 :to n)
                                        (unless (first-iteration-p)
                                          (collect " OR "))
                                        (collect (format nil "(a = (b + $~d::NUMERIC + ~d))" i i)))))))
    (bind ((criteria [or ,@(iter (for i :from 1 :to n)
                                 (rebind (i)
                                   (collect (sql-= (sql-identifier :name 'a)
                                                   (sql-+ (sql-identifier :name 'b)
                                                          (sql-binding-variable
                                                            :type (sql-integer-type)
                                                            :name i)
                                                          i)))))]))
      (is (string=
           expected
           (funcall
            [select (a b) t ,criteria]))))
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

(def ast-dialect-test test/syntax/formatting/identifier
  (sql-identifier :name "alma")
  "alma"

  (sql-identifier :name 'alma)
  "alma")

(def ast-dialect-test test/syntax/formatting/create-table
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

(def ast-dialect-test test/syntax/formatting/alter-table
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

(def ast-dialect-test test/syntax/formatting/drop-table
  (sql-drop-table :name "a")
  "DROP TABLE a")

(def ast-dialect-test test/syntax/formatting/create-index
  (sql-create-index :name "a"
                    :table-name "a"
                    :columns (list "a" "a"))
  "CREATE INDEX a ON a (a, a)")

(def ast-dialect-test test/syntax/formatting/drop-index
  (sql-drop-index :name "a")
  "DROP INDEX a")

(def ast-dialect-test test/syntax/formatting/insert
  (sql-insert :table "a"
              :columns (list "a")
              :values (list "a"))
  "INSERT INTO a (a) VALUES ('a')"

  (sql-insert :table (sql-identifier :name "a")
              :columns (list (sql-identifier :name "a"))
              :values (list "a"))
  "INSERT INTO a (a) VALUES ('a')")

(def ast-dialect-test test/syntax/formatting/select
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

(def ast-dialect-test test/syntax/formatting/update
  (sql-update :table "a"
              :columns (list "a")
              :values (list "a"))
  "UPDATE a SET a = 'a'"

  (sql-update :table (sql-identifier :name "a")
              :columns (list (sql-identifier :name "a"))
              :values (list "a"))
  "UPDATE a SET a = 'a'")

(def ast-dialect-test test/syntax/formatting/delete
  (sql-delete :table "a")
  "DELETE from a"

  (sql-delete :table (make-instance 'sql-identifier :name "a"))
  "DELETE from a")

(def ast-dialect-test test/syntax/formatting/sequence
  (sql-create-sequence :name "a")
  "CREATE SEQUENCE a"

  (sql-drop-sequence :name "a")
  "DROP SEQUENCE a"

  (sql-select :columns (list (sql-sequence-nextval-column :name "a")))
  ((oracle "SELECT a.nextval FROM dual ")
   (t "SELECT NEXTVAL('a')")))

#|
this variant runs the tests for all dialects, but it's not useful because
that would require all the backends to be loaded (for their syntax customizations)
which can be a headache for complex backends like the oracle one.

(def definer dialect-test (name sexp-p &body body)
  `(def test ,name ()
     ,@(flet ((construct-entry (database expected)
                `(is (equalp
                      (with-database
                          (closer-mop:class-prototype
                           (find-class
                            ',(if (eq database t)
                                  'database
                                  database)))
                        (format-sql-to-string sql-ast))
                      ,expected))))
         (iter (for (sql cases) :on body :by #'cddr)
               (collect `(bind ((sql-ast ,(if sexp-p
                                              `(compile-sexp-sql ,sql)
                                              sql)))
                           ,@(if (stringp cases)
                                 (list (construct-entry t cases))
                                 (iter (for (database expected) :in cases)
                                       (collect (construct-entry database expected))))))))))
|#
