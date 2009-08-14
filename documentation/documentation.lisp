;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.rdbms.documentation)

;; TODO: move these to source?

(def dictionary database ()
  *database*
  database
  transaction-mixin-class
  with-database)

(def dictionary transaction ()
  *transaction*
  begin
  begin-transaction
  call-in-transaction
  cleanup-transaction
  commit
  commit-transaction
  in-transaction-p
  make-transaction
  mark-transaction-for-commit-only
  mark-transaction-for-rollback-only
  rollback
  rollback-transaction
  transaction
  transaction-error
  transaction-in-progress-p
  transaction-valid-p
  with-readonly-transaction
  with-transaction
  with-transaction*)

(def dictionary transaction-hook ()
  register-hook-in-transaction
  register-transaction-hook)

(def dictionary execute ()
  break-on-next-command
  current-delete-counter
  current-insert-counter
  current-select-counter
  current-update-counter
  execute
  execute-command
  execute-ddl
  report-transaction-state)

(def dictionary command ()
  delete-records
  insert-record
  select-count-*
  select-records
  update-records)

(def dictionary cursor ()
  collect-rows
  column-count
  column-name
  column-type
  column-value
  current-row
  cursor-position
  for-each-row
  make-cursor
  row-count)

(def dictionary table ()
  create-table
  create-temporary-table
  drop-table
  alter-table
  list-tables
  list-table-columns
  table-exists-p
  update-table)

(def dictionary column ()
  add-column
  drop-column
  alter-column-type
  add-primary-key-constraint
  list-table-columns)

(def dictionary view ()
  create-view
  drop-view
  list-dependent-views
  list-views
  update-view
  view-exists-p)

(def dictionary sequence ()
  create-sequence
  drop-sequence
  list-sequences
  sequence-exists-p
  sequence-next)

(def dictionary index ()
  create-index
  drop-index
  list-table-indices
  update-index)
