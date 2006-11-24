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

(defmacro syntax-test (name &body body)
  `(test (,name)
    (with-database *test-database*
      ,@ (loop for (sql string) :on body :by #'cddr
               collect `(is (equalp
                             (format-sql-to-string (sql ,sql))
                             ,string))))))

(syntax-test syntax
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
  "SELECT foo.column, bar FROM alma AS alma_alias")
