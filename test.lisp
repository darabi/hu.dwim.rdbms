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
                                with-transaction* process-sql-syntax-list compile-sql-column compile-sql-columns compile-sql-type
                                value-of compile-sql-binding-variable compile-sql-literal
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
  (destructuring-bind (name &rest args &key depends-on &allow-other-keys) (ensure-list name)
    (remf-keywords args :depends-on)
    `(test (,name :depends-on (and ,@(ensure-list depends-on) connect) ,@args)
      (with-database *test-database*
        ,@body))))

(test connect
  (finishes
    (with-database *test-database*
      (with-transaction
        (execute "select now()"))
      (ignore-errors
        (execute-ddl "DROP TABLE alma")))))

(test* create-table
  (finishes
    (unwind-protect
         (execute-ddl "CREATE TABLE alma ()")
      (ignore-errors
        (execute-ddl "DROP TABLE alma")))))

(test* encoding
  (let ((unicode-text "éáúóüőű"))
    (unwind-protect
         (with-transaction
           (execute-ddl "CREATE TABLE alma (name varchar(40))")
           (execute (format nil "INSERT INTO alma VALUES ('~A')" unicode-text))
           (is (string= (first (first (execute "SELECT name FROM alma"))) unicode-text)))
      (ignore-errors
        (execute-ddl "DROP TABLE alma")))))

(test* basic-binding
  (let ((unicode-text "éáúóüőű"))
    (unwind-protect
         (with-transaction
           (execute-ddl (sql `(create table alma ((name (varchar 50))))))
           (execute (sql `(insert alma (name) ((,unicode-text varchar)))))
           (is (string= (first (first (execute "SELECT * FROM alma"))) unicode-text)))
      (ignore-errors
        (execute-ddl "DROP TABLE alma")))))

(test* binding
  (let* ((columns (compile-sql-columns `((a (integer 32))
                                         (string_column (varchar 50))
                                         (integer_column (integer 32))
                                         (b (integer 32)))))
         (binding-literals (loop for entry :in `(("éáúóüőű" varchar)
                                                 (42 (integer 32)))
                                 for value = (if (consp entry) (first entry) entry)
                                 for type = (when (consp entry) (second entry))
                                 for idx :upfrom 0
                                 collect (progn
                                           (setf type (if type
                                                          (compile-sql-type type)
                                                          (type-of (elt columns idx))))
                                           (make-instance 'sql-literal :value value :type type)))))
    (unwind-protect
         (with-transaction
           (execute-ddl (sql `(create table alma ,columns)))
           (execute (sql `(insert alma ,columns ,(append (list (compile-sql-literal '(? named1 (integer 32))))
                                                         binding-literals
                                                         (list (compile-sql-literal '(? named2 (integer 32)))))))
                    :bindings `(named1 11
                                named2 22))
           (execute (sql `(select ,columns alma))
                    :visitor (let ((first-time #t))
                               (lambda (row)
                                 (unless first-time
                                   (fail "Multiple rows?!"))
                                 (setf first-time #f)
                                 (is (= (pop row) 11))
                                 (is (string= (pop row) (value-of (first binding-literals))))
                                 (is (= (pop row) (value-of (second binding-literals))))
                                 (is (= (pop row) 22)))))
           (signals unbound-binding-variable-error
             (execute (sql `(insert alma ,columns ,(append (list (compile-sql-literal '(? named1 (integer 32))))
                                                           binding-literals
                                                           (list (compile-sql-literal '(? named2 (integer 32))))))))))
      (ignore-errors
        (execute-ddl "DROP TABLE alma")))))

(test* terminal-action
  (unwind-protect
       (progn
         (execute-ddl "CREATE TABLE alma (x integer)")
         (with-transaction
           (execute "INSERT INTO alma VALUES (42)")
           (is (= (first (first (execute "SELECT x FROM alma"))) 42))
           (mark-transaction-for-rollback-only))
         (with-transaction
           (is (zerop (first (first (execute "SELECT count(*) FROM alma")))) "mark-transaction-for-rollback-only had no effects?"))
         (with-transaction
           (execute "INSERT INTO alma VALUES (42)"))
         (with-transaction
           (is (= 1 (first (first (execute "SELECT count(*) FROM alma")))) "with-transaction didn't commit as a terminal action?")))
    (ignore-errors
      (execute-ddl "DROP TABLE alma"))))

(test* insert-records
  (unwind-protect
       (let ((columns (compile-sql-columns
                       `((a (integer 32))
                         (b (varchar 50))))))
         (create-table 'alma columns)
         (with-transaction
           (insert-records 'alma columns (list 1 "alma"))
           (is (equal '((1 "alma")) (select-records columns '(alma))))))
    (ignore-errors
      (execute-ddl "DROP TABLE alma"))))

(test* update-records
  (unwind-protect
       (let ((columns (compile-sql-columns
                       `((a (integer 32))
                         (b (varchar 50))))))
         (create-table 'alma columns)
         (with-transaction
           (execute "insert into alma values (NULL, NULL)")
           (update-records 'alma columns (list 1 "alma"))
           (is (equal '((1 "alma")) (select-records columns '(alma))))))
    (ignore-errors
      (execute-ddl "DROP TABLE alma"))))

(defmacro syntax-test* (name sexp-p &body body)
  `(test (,name)
    (with-database *test-database*
      ,@ (loop for (sql string) :on body :by #'cddr
               collect `(is (equalp
                             (format-sql-to-string ,(if sexp-p
                                                        `(sql ,sql)
                                                        sql))
                             ,string))))))

(defmacro syntax-test (name &body body)
  `(syntax-test* ,name #t ,@body))

(defmacro ast-test (name &body body)
  `(syntax-test* ,name #f ,@body))

(ast-test identifier-syntax
  (make-instance 'sql-identifier :name "a")
  "a"

  (make-instance 'sql-identifier :name 'a)
  "a")

(ast-test create-table-syntax
  (make-instance 'sql-create-table
                 :name "a"
                 :columns (list (make-instance 'sql-column :name "a" :type (make-instance 'sql-integer-type))))
  "CREATE TABLE a (a NUMERIC)"

  (make-instance 'sql-create-table
                 :temporary :drop
                 :name "a"
                 :columns (list (make-instance 'sql-column :name "a" :type (make-instance 'sql-integer-type))))
  "CREATE TEMPORARY TABLE a (a NUMERIC) ON COMMIT DROP")

(ast-test alter-table-syntax
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

(ast-test drop-table-syntax
  (make-instance 'sql-drop-table
                 :name "a")
  "DROP TABLE a")

(ast-test insert-syntax
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

(ast-test select-syntax
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
  
(ast-test update-syntax
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
  
(ast-test delete-syntax
  (make-instance 'sql-delete
                 :table "a")
  "DELETE from a"

  (make-instance 'sql-delete
                 :table (make-instance 'sql-identifier :name "a"))
  "DELETE from a")

(ast-test sequence-syntax
  (make-instance 'sql-create-sequence
                 :name "a")
  "CREATE SEQUENCE a"

  (make-instance 'sql-drop-sequence
                 :name "a")
  "DROP SEQUENCE a"

  (make-instance 'sql-select
                 :columns (list (make-instance 'sql-sequence-nextval-column :name "a")))
  "SELECT NEXTVAL('a')")

(syntax-test sexp-syntax
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
  "SELECT foo.column, bar FROM alma alma_alias"

  `(create table (:temporary :drop) alma ((col1 varchar) ("col2" (integer 32))))
  "CREATE TEMPORARY TABLE alma (col1 CHARACTER VARYING, col2 INT) ON COMMIT DROP"

  `(create table (:temporary :delete-rows) alma (("col2" (integer 32))))
  "CREATE TEMPORARY TABLE alma (col2 INT) ON COMMIT DELETE ROWS")
