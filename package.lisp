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
   #:execute
   #:execute-ddl
   #:with-transaction
   #:in-transaction-p
   #:transaction-in-progress-p

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
   #:unconfirmed-destructive-alter-table-error
   #:unconfirmed-destructive-alter-column-type-error
   #:unconfirmed-destructive-drop-column-error

   #:create-sequence
   #:drop-sequence
   #:sequence-exists-p
   #:sequence-next

   #:sql
   #:sql*
   #:format-sql
   #:format-sql-to-string
   #:import-sql-syntax-node-names

   #:mark-transaction-for-commit-only
   #:mark-transaction-for-rollback-only

   #:insert-records
   #:update-records
   #:delete-records
   #:select-records)

  ;; for debug purposes
  (:export
   #:begin
   #:commit
   #:rollback

   #:start-sql-recording
   #:stop-sql-recording

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
  :level +warn+
  :compile-time-level #+debug +dribble+ #-debug +warn+
  :appender (make-instance 'brief-stream-log-appender :stream *debug-io*))

(defclass sql-log-appender (stream-log-appender)
  ())

(deflogger sql-log ()
  :level +warn+
  :compile-time-level +info+
  :appender (make-instance 'sql-log-appender :stream *debug-io*))

(defun start-sql-recording ()
  (setf (log.level (get-logger 'sql-log)) +info+)
  (values))

(defun stop-sql-recording ()
  (setf (log.level (get-logger 'sql-log)) +warn+)
  (values))

(defmethod append-message ((category log-category) (appender sql-log-appender) message level)
  (format (arnesi::log-stream appender) "~A~%" message))
