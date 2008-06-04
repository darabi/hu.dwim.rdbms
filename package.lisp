;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-user)

(defpackage :cl-rdbms
  (:nicknames :rdbms)

  (:shadow #:log #:type-of #:type)

  ;; TODO drop :arnesi, use :alexandria
  (:use
   #:common-lisp
   #:sb-pcl
   :iterate
   :arnesi
   :cl-def
   :metabang-bind
   :defclass-star
   :cl-rdbms-system
   :cl-syntax-sugar
   )

  (:shadowing-import-from :cl-syntax-sugar
   ;; some clashes with arnesi
   #:enable-sharp-l-syntax
   #:with-sharp-l-syntax
   #:with-package
   )

  (:export
   #:rdbms-error
   #:unable-to-obtain-lock-error

   #:database
   #:postgresql
   #:postgresql-postmodern
   #:oracle
   #:sqlite
   #:*database*
   #:with-database
   #:transaction
   #:*transaction*
   #:execute
   #:execute-ddl

   #:with-transaction
   #:with-transaction*
   #:call-in-transaction
   #:make-transaction
   #:begin-transaction
   #:commit-transaction
   #:rollback-transaction
   #:cleanup-transaction
   #:notify-transaction-event
   #:in-transaction-p
   #:transaction-in-progress-p
   #:transaction-valid-p
   #:register-transaction-hook
   #:transaction-with-hooks-mixin
   #:transaction-timestamp
   #:rdbms-name-for

   #:table-exists-p
   #:unconfirmed-destructive-alter-table-error
   #:unconfirmed-destructive-alter-column-type-error
   #:unconfirmed-destructive-drop-column-error
   #:unbound-binding-variable-error
   #:with-confirmed-descructive-changes

   #:create-view
   #:drop-view
   #:update-view
   #:list-views
   #:view-exists-p

   #:create-sequence
   #:drop-sequence
   #:sequence-exists-p
   #:sequence-next

   #:sql
   #:format-sql
   #:format-sql-to-string
   #:import-sql-syntax-node-names
   #:import-sql-constructor-names

   #:sql-cond
   #:sql-if

   #:mark-transaction-for-commit-only
   #:mark-transaction-for-rollback-only

   ;; well, these are... but if you want to deal with sequences...
   #:first*
   #:second*
   #:third*
   #:last*

   #:insert-record
   #:update-records
   #:delete-records
   #:select-records
   #:select-count-*

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

  (:use
   #:common-lisp
   :iterate
   :arnesi
   :cl-rdbms
   :cl-def
   :metabang-bind
   )

  (:shadowing-import-from :cl-rdbms
   #:log))
