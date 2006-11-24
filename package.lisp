;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-user)

(defpackage :cl-rdbms
    (:shadow #:log #:type-of)
  
  (:use :cl :cl-rdbms-system :arnesi :defclass-star)

  (:export
   #:database
   #:*database*
   #:with-database
   #:postgresql
   #:postgresql-pg
   #:transaction
   #:*transaction*
   #:begin
   #:commit
   #:rollback
   #:execute
   #:in-transaction-p
   #:transaction-in-progress-p
   #:with-transaction

   #:create-table
   #:drop-table
   #:alter-table
   #:add-column
   #:drop-column
   #:alter-column-type
   #:update-table
   #:list-tables
   #:list-table-columns
   #:table-exists-p

   #:sql
   #:sql*
   #:format-sql
   #:format-sql-to-string
   #:import-sql-syntax-node-names

   #:insert-records
   #:update-records
   #:delete-records
   #:select-records)

  ;; for debug purposes
  (:export
   #:command-counter-of
   #:insert-counter-of
   #:select-counter-of
   #:update-counter-of
   #:delete-counter-of))

(defpackage :cl-rdbms-test
  (:use :cl :cl-rdbms :arnesi)
  (:shadowing-import-from :cl-rdbms
                          #:log))

(in-package :cl-rdbms)

(deflogger log ()
  :level #+debug +dribble+ #-debug +warn+
  :compile-time-level #+debug +dribble+ #-debug +warn+
  :appender (make-instance 'brief-stream-log-appender :stream *debug-io*))
