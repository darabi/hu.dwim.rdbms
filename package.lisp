;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-user)

(defpackage :cl-rdbms
  (:nicknames :rdbms)

  (:shadow #:log #:type-of)
  
  (:use :cl :sb-pcl :cl-rdbms-system :arnesi :defclass-star)

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
   #:with-transaction*
   #:in-transaction-p
   #:transaction-in-progress-p
   #:register-commit-hook
   #:transaction-with-commit-hooks-mixin

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
   #:unbound-binding-variable-error
   #:with-confirmed-descructive-changes

   #:create-sequence
   #:drop-sequence
   #:sequence-exists-p
   #:sequence-next

   #:create-index
   #:create-index*
   #:drop-index
   #:update-index
   #:update-index*
   #:list-table-indices

   #:sql
   #:sql*
   #:format-sql
   #:format-sql-to-string
   #:import-sql-syntax-node-names
   #:import-sql-constructor-names

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
   #:delete-counter-of
   #:current-insert-counter
   #:current-select-counter
   #:current-update-counter
   #:current-delete-counter))

(defpackage :cl-rdbms-test
  (:nicknames :rdbmst)

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
  (format (arnesi::log-stream appender) "~&~A~%" message))
