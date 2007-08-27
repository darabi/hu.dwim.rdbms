;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms-test)

#.(cl-rdbms::file-header)

(enable-sql-syntax)

;;; please note that cl-perec tests cl-rdbms throughout, this is only a limited test file here

(eval-always
  (use-package :stefil)
  (import (let ((*package* (find-package :cl-rdbms)))
            ;; we intentionally not import all internal cl-rdbms symbols here to test the proper exporting of the symbols, too
            (read-from-string "(enable-sharp-boolean-syntax
                                connection-specification-of *database* *transaction*
                                with-transaction* process-sql-syntax-list compile-sexp-sql-column compile-sexp-sql-columns
                                compile-sexp-sql compile-sexp-sql-type
                                value-of compile-sexp-sql-binding-variable compile-sexp-sql-literal first* second* third*
                                log log.dribble log.debug log.info log.warn log.error)")))
  (import-sql-syntax-node-names))

(in-root-suite)

(defsuite* test)

;; e.g. (make-instance 'postgresql-postmodern :connection-specification '(:database "dwim" :user-name "root" :password "admin123"))

(defvar *test-database*)

(defmacro with-test-transaction (&body body)
  `(with-transaction* (:database *test-database*)
    ,@body))

(defmacro deftest* (name args &body body)
  `(deftest ,name ,args
    (with-database *test-database*
      ,@body)))

(deftest* test/connect ()
  (finishes
    (with-transaction
      (execute "set transaction read only"))))

(deftest* test/create-table ()
  (finishes
    (unwind-protect
         (execute-ddl "CREATE TABLE alma (name varchar(40))")
      (ignore-errors
        (execute-ddl "DROP TABLE alma")))))

(deftest* test/encoding ()
  (let ((unicode-text "éáúóüőű"))
    (unwind-protect
         (with-transaction
           (execute-ddl "CREATE TABLE alma (name varchar(40))")
           (execute (format nil "INSERT INTO alma VALUES ('~A')" unicode-text))
           (is (string= (first* (first* (execute "SELECT name FROM alma"))) unicode-text)))
      (ignore-errors
        (execute-ddl "DROP TABLE alma")))))

(deftest* test/basic-binding ()
  (let ((unicode-text "éáúóüőű"))
    (unwind-protect
         (with-transaction
           (execute-ddl [create table alma ((name (varchar 50)))])
           (execute [insert alma (name) ((,unicode-text varchar))])
           (is (string= (first* (first* (execute [select * alma]))) unicode-text)))
      (ignore-errors
        (execute-ddl [drop table alma])))))

(deftest* test/binding ()
  (let* ((columns (compile-sexp-sql-columns
                   `((a (integer 32))
                     (string_column (varchar 50))
                     (integer_column (integer 32))
                     (boolean_true_column boolean)
                     (boolean_false_column boolean)
                     (b (integer 32)))))
         (binding-literals (loop for entry :in `(("éáúóüőű" varchar)
                                                 (42 (integer 32))
                                                 (#t boolean)
                                                 (#f boolean))
                                 for value = (if (consp entry) (first entry) entry)
                                 for type = (when (consp entry) (second entry))
                                 for idx :upfrom 0
                                 collect (progn
                                           (setf type (if type
                                                          (compile-sexp-sql-type type)
                                                          (type-of (elt columns idx))))
                                           (make-instance 'sql-literal :value value :type type)))))
    (unwind-protect
         (with-transaction
           (execute-ddl [create table alma ,columns])
           (execute [insert alma
                            ,columns
                            ,(append (list (compile-sexp-sql-literal '(? named1 (integer 32))))
                                     binding-literals
                                     (list (compile-sexp-sql-literal '(? named2 (integer 32)))))]
                    :bindings `(named1 11
                                named2 22))
           (execute [select ,columns alma]
                    :visitor (let ((first-time #t))
                               (lambda (row)
                                 (let ((idx -1))
                                   (flet ((next ()
                                            (elt row (incf idx))))
                                     (is first-time)
                                     (setf first-time #f)
                                     (is (eql (next) 11))
                                     (is (string= (next) (value-of (first binding-literals))))
                                     (is (eql (next) (value-of (second binding-literals))))
                                     (is (eql (next) (value-of (third binding-literals))))
                                     (is (eql (next) (value-of (fourth binding-literals))))
                                     (is (eql (next) 22)))))))
           (signals unbound-binding-variable-error
             (execute [insert alma
                              ,columns
                              ,(append (list (compile-sexp-sql-literal '(? named1 (integer 32))))
                                       binding-literals
                                       (list (compile-sexp-sql-literal '(? named2 (integer 32)))))])))
      (ignore-errors
        (execute-ddl [drop table alma])))))

(defmacro define-type-test (name type &body values)
  `(deftest ,name ()
    (unwind-protect
         (let* ((values (list ,@values))
                (literals (mapcar
                           (lambda (value)
                             (sql-literal :value value
                                          :type (compile-sexp-sql-type ',type)))
                           values)))
           (with-transaction
             ;; the reader syntax can not cope with backquote, so we use SQL
             (execute-ddl (sql (create table alma ((a ,type)))))
             (loop for literal in literals
                   do (execute (sql (insert alma (a) ((sql-unquote literal))))))
             (if (and values (typep (first values) 'local-time:local-time))
                 (is
                  (every #'local-time:local-time=
                         (apply #'nconc (execute [select * alma] :result-type 'list))
                         values))
                 (is
                  (equalp
                   (apply #'nconc (execute [select * alma] :result-type 'list))
                   values)))))
    
      (ignore-errors
        (execute-ddl [drop table alma])))))

(define-type-test test/boolean boolean
  t
  nil)

(define-type-test test/char (char 10)
  "1234567890"
  "áéíóöőúüű ")

(define-type-test test/varchar (varchar 10)
  "1234567890"
  "áéíóöőúüű")

(define-type-test test/clob clob
  "1234567890"
  "áéíóöőúüű")

(define-type-test test/int8 (integer 8)
  0
  1
  -1
  127
  -128)

(define-type-test test/int16 (integer 16)
  0
  1
  -1
  32767
  -32768)

(define-type-test test/int32 (integer 32)
  0
  1
  -1
  2147483647
  -2147483648)

(define-type-test test/integer integer
  0
  1
  -1
  12345678901234567890123456789012345678
  -12345678901234567890123456789012345678)

(define-type-test test/date date
  (local-time:today))

;; TODO time type is not gonna work with local-time, or needs special support
(define-type-test test/time time
  (local-time:parse-timestring "06:06:06Z"))

(define-type-test test/timestamp (timestamp #f)
  (local-time:parse-timestring "2006-06-06T06:06:06Z"))

(define-type-test test/timestamp-tz (timestamp #t)
  (local-time:parse-timestring "2006-06-06T06:06:06Z")
  (local-time:parse-timestring "2006-06-06T06:06:06-01:30")
  (local-time:parse-timestring "2006-06-06T06:06:06+01:25")
  (local-time:now))

(define-type-test test/blob blob
  (coerce #(1 2 3 4 5 6 7 8 9 0) '(vector (unsigned-byte 8))))


(deftest* test/terminal-action ()
  (unwind-protect
       (progn
         (execute-ddl "CREATE TABLE alma (x integer)")
         (with-transaction
           (execute "INSERT INTO alma VALUES (42)")
           (is (= (first* (first* (execute "SELECT x FROM alma"))) 42))
           (mark-transaction-for-rollback-only))
         (with-transaction
           (is (zerop (first* (first* (execute "SELECT count(*) FROM alma"))))))
         (with-transaction
           (execute "INSERT INTO alma VALUES (42)"))
         (with-transaction
           (is (= 1 (first* (first* (execute "SELECT count(*) FROM alma")))))))
    (ignore-errors
      (execute-ddl "DROP TABLE alma"))))

(deftest* test/insert-record ()
  (unwind-protect
       (let ((columns (compile-sexp-sql-columns
                       `((a (integer 32))
                         (b (varchar 50))))))
         (create-table 'alma columns)
         (with-transaction
           (insert-record 'alma columns (list 1 "alma"))
           (let ((row (first* (select-records columns '(alma)))))
             (is (= (elt row 0) 1))
             (is (string= (elt row 1) "alma")))))
    (ignore-errors
      (execute-ddl [drop table alma]))))

(deftest* test/update-records ()
  (unwind-protect
       (let ((columns (compile-sexp-sql-columns
                       `((a (integer 32))
                         (b (varchar 50))))))
         (create-table 'alma columns)
         (with-transaction
           (execute [insert alma (a b) (:null :null)])
           (update-records 'alma columns (list 1 "alma"))
           (let ((row (first* (select-records columns '(alma)))))
             (is (= (elt row 0) 1))
             (is (string= (elt row 1) "alma")))))
    (ignore-errors
      (execute-ddl [drop table alma]))))

(defmacro defsyntaxtest* (name sexp-p &body body)
  `(deftest ,name ()
    (with-database *test-database*
      ,@ (loop for (sql string-or-case-body) :on body :by #'cddr
               collect `(is (equalp
                             (format-sql-to-string ,(if sexp-p
                                                        `(compile-sexp-sql ,sql)
                                                        sql))
                             ,(if (stringp string-or-case-body)
                                  string-or-case-body
                                  `(typecase *database*
                                    ,@string-or-case-body))))))))

(defmacro defsyntaxtest (name &body body)
  `(defsyntaxtest* ,name #t ,@body))

(defmacro defasttest (name &body body)
  `(defsyntaxtest* ,name #f ,@body))

(defasttest test/identifier-syntax
  (make-instance 'sql-identifier :name "a")
  "a"

  (make-instance 'sql-identifier :name 'a)
  "a")

(defasttest test/create-table-syntax
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

(defasttest test/alter-table-syntax
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

(defasttest test/drop-table-syntax
  (make-instance 'sql-drop-table
                 :name "a")
  "DROP TABLE a")

(defasttest test/create-index-syntax
  (make-instance 'sql-create-index
                 :name "a"
                 :table-name "a"
                 :columns (list "a" "a"))
  "CREATE INDEX a ON a (a, a)")

(defasttest test/drop-index-syntax
  (make-instance 'sql-drop-index
                 :name "a")
  "DROP INDEX a")

(defasttest test/insert-syntax
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

(defasttest test/select-syntax
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
  
(defasttest test/update-syntax
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
  
(defasttest test/delete-syntax
  (make-instance 'sql-delete
                 :table "a")
  "DELETE from a"

  (make-instance 'sql-delete
                 :table (make-instance 'sql-identifier :name "a"))
  "DELETE from a")

(defasttest test/sequence-syntax
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

(defsyntaxtest test/sexp-syntax
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
