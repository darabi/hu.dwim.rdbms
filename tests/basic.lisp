;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms-test)

#.(cl-rdbms::file-header)

(enable-sql-syntax)

(def suite (basic :in-suite 'test))

(in-suite basic)

(def test* test/basic/connect ()
  (finishes
    (with-transaction
      (execute "set transaction read only"))))

(def test* test/basic/create-table ()
  (finishes
    (unwind-protect
         (execute-ddl "CREATE TABLE alma (name varchar(40))")
      (ignore-errors
        (execute-ddl "DROP TABLE alma")))))

(def test* test/basic/encoding ()
  (bind ((unicode-text "éáúóüőű"))
    (unwind-protect
         (with-transaction
           (execute-ddl "CREATE TABLE alma (name varchar(40))")
           (execute (format nil "INSERT INTO alma VALUES ('~A')" unicode-text))
           (is (string= (first* (first* (execute "SELECT name FROM alma"))) unicode-text)))
      (ignore-errors
        (execute-ddl "DROP TABLE alma")))))

(def test* test/basic/basic-binding ()
  (bind ((unicode-text "éáúóüőű"))
    (unwind-protect
         (with-transaction
           (execute-ddl [create table alma ((name (varchar 50)))])
           (execute [insert alma (name) ((,unicode-text varchar))])
           (is (string= (first* (first* (execute [select * alma]))) unicode-text)))
      (ignore-errors
        (execute-ddl [drop table alma])))))

(def test* test/basic/binding ()
  (bind ((columns (compile-sexp-sql-columns
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

(def test* test/basic/terminal-action ()
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

(def test* test/basic/insert-record ()
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

(def test* test/basic/update-records ()
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