;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-user)

(defpackage :cl-rdbms
  (:nicknames :rdbms)

  (:shadow #:log #:type-of #:type)
  
  (:use :cl :sb-pcl :iterate :arnesi :defclass-star :cl-rdbms-system)

  (:export
   #:database
   #:oracle
   #:postgresql
   #:postgresql-pg
   #:postgresql-postmodern
   #:*database*
   #:with-database
   #:transaction
   #:*transaction*
   #:execute
   #:execute-ddl
   #:with-transaction
   #:with-transaction*
   #:in-transaction-p
   #:transaction-in-progress-p
   #:transaction-valid-p
   #:register-commit-hook
   #:transaction-with-commit-hooks-mixin
   #:transaction-timestamp
   #:rdbms-name-for

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

   #:sql-cond
   #:sql-if

   #:mark-transaction-for-commit-only
   #:mark-transaction-for-rollback-only

   #:insert-record
   #:update-records
   #:delete-records
   #:select-records

   #:make-cursor
   #:cursor-position
   #:column-count
   #:row-count
   #:column-name
   #:column-type
   #:column-value
   #:for-each-row
   #:collect-rows
   #:current-row)

  ;; for debug purposes
  (:export
   #:begin
   #:commit
   #:rollback

   #:start-sql-recording
   #:stop-sql-recording
   #:enable-sql-recording
   #:disable-sql-recording

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

  (:use :cl :iterate :arnesi :cl-rdbms)

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

(defun enable-sql-recording ()
  (start-sql-recording))

(defun disable-sql-recording ()
  (stop-sql-recording))

(defmethod append-message ((category log-category) (appender sql-log-appender) message level)
  (format (arnesi::log-stream appender) "~&~A~%" message))
