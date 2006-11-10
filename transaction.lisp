;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms)

#.(file-header)

(defvar *transaction*)

(defcondition* transaction-error (simple-error)
  ())

(defclass* transaction ()
  ((database)
   (state
    :uninitialized
    :type (member :uninitialized :committed :rolled-back :in-progress)))
  (:documentation "An object representing a transaction context. The actual backend connection/transaction is lazily created."))

(defmacro with-transaction (&body body)
  `(with-transaction* ()
    ,@body))

(defmacro with-transaction* ((&key (database *database*)) &body body)
  (with-unique-names (body-finished-p)
    `(let ((*transaction* nil)
           (,body-finished-p #f))
      (unwind-protect
           (let ((*database* ,database))
             (setf *transaction* (begin))
             (multiple-value-prog1
                 (progn
                   ,@body)
               (setf ,body-finished-p #t)
               (commit)))
        (unless ,body-finished-p
          (rollback))
        (when *transaction*
          (cleanup-transaction *transaction*))))))

(defun in-transaction-p ()
  (and (boundp '*transaction*)
       *transaction*
       (eq (state-of *transaction*) :in-progress)))

(defun assert-transaction-in-progress ()
  (unless (in-transaction-p)
    (error 'transaction-error :format-control "No transaction in progress")))

(defun begin ()
  (assert (not (in-transaction-p))
          () "Nested transactions are not yet supported")
  (begin-transaction *database*))

(defun commit ()
  (assert-transaction-in-progress)
  (when (transaction-connected-p *transaction*)
    (commit-transaction *database* *transaction*)))

(defun rollback ()
  (assert-transaction-in-progress)
  (when (transaction-connected-p *transaction*)
    (rollback-transaction *database* *transaction*)))

(defun execute (command)
  (assert-transaction-in-progress)
  (execute-command *database* *transaction* command))

(defgeneric begin-transaction (database)
  (:method :around (database)
           (aprog1
               (call-next-method)
             (setf (database-of it) database)
             (setf (state-of it) :in-progress))))

(defgeneric commit-transaction (database transaction)
  (:method :around (database transaction)
           (aprog1
               (call-next-method)
             (log.dribble "Commit returned ~A" it)
             (setf (state-of transaction) :committed))))

(defgeneric rollback-transaction (database transaction)
  (:method :around (database transaction)
           (aprog1
               (call-next-method)
             (log.dribble "Rollback returned ~A" it)
             (setf (state-of transaction) :rolled-back))))

(defgeneric transaction-connected-p (transaction)
  (:method (tr)
           #t))

(defgeneric cleanup-transaction (transaction))

(defgeneric execute-command (database transaction command)
  #+debug
  (:method :before (database transaction command)
           (log.dribble "Executing command ~A in transaction ~A of database ~A"
                        command transaction database)))

