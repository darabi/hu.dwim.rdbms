;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms)

#.(file-header)

(defvar *transaction*)

(defcondition* transaction-error (simple-rdbms-error)
  ())

(defclass* transaction ()
  ((database
    :type database)
   (command-counter
    (make-instance 'command-counter)
    :type command-counter)
   (state
    :uninitialized
    :type (member :uninitialized :committed :rolled-back :in-progress)))
  (:documentation "An object representing a transaction context. The actual backend connection/transaction is usually lazily created."))

(defclass* command-counter ()
  ((insert-counter 0 :type integer)
   (select-counter 0 :type integer)
   (update-counter 0 :type integer)
   (delete-counter 0 :type integer)))

(defprint-object (self command-counter)
  (princ (strcat "insert: " (insert-counter-of self)
                 " select: " (select-counter-of self)
                 " update: " (update-counter-of self)
                 " delete: " (delete-counter-of self))))

(defmacro with-transaction (&body body)
  `(with-transaction* ()
    ,@body))

(defmacro with-transaction* ((&key database) &body body)
  (with-unique-names (body-finished-p)
    `(let ((*transaction* nil)
           ,@(when database `((*database* ,database)))
           (,body-finished-p #f))
      (unwind-protect
           (progn
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
       (transaction-in-progress-p *transaction*)))

(defun transaction-in-progress-p (&optional (transaction *transaction*))
  (eq (state-of transaction) :in-progress))

(defun assert-transaction-in-progress ()
  (unless (in-transaction-p)
    (error 'transaction-error :format-control "No transaction in progress")))

(defun begin ()
  (assert (not (in-transaction-p))
          () "Nested transactions are not yet supported")
  (begin-transaction *database*))

(defun commit ()
  (assert-transaction-in-progress)
  (commit-transaction *database* *transaction*))

(defun rollback ()
  (assert-transaction-in-progress)
  (rollback-transaction *database* *transaction*))

(defun execute (command &optional visitor)
  (assert-transaction-in-progress)
  (execute-command *database* *transaction* command visitor))

(defmethod transaction-class-name list (database)
  'transaction)

(defgeneric begin-transaction (database)
  (:method :around (database)
           (log.debug "About to BEGIN transaction in database ~A" database)
           (aprog1
               (call-next-method)
             (setf (database-of it) database)
             (setf (state-of it) :in-progress)))

  (:method (database)
           (make-instance (transaction-class-of database))))

(defgeneric commit-transaction (database transaction)
  (:method :around (database transaction)
           (log.debug "About to COMMIT transaction ~A" transaction)
           (when (transaction-connected-p transaction)
             (call-next-method))
           (setf (state-of transaction) :committed)
           (values)))

(defgeneric rollback-transaction (database transaction)
  (:method :around (database transaction)
           (log.dribble "About to ROLLBACK transaction ~A" transaction)
           (when (transaction-connected-p transaction)
             (call-next-method))
           (setf (state-of transaction) :rolled-back)
           (values)))

(defgeneric transaction-connected-p (transaction)
  (:method (tr)
           #t))

(defgeneric cleanup-transaction (transaction))

(defgeneric execute-command (database transaction command &optional visitor)
  #+debug
  (:method :before (database transaction command &optional visitor)
           (declare (ignore visitor))
           (log.dribble "*** ~S in transaction ~A of database ~A"
                        command transaction database))

  (:method :after (database transaction (command string) &optional visitor)
           (declare (ignore visitor))
           (let ((command-counter (command-counter-of transaction)))
             (cond ((starts-with command "INSERT" :test #'equalp)
                   (incf (insert-counter-of command-counter)))
                  ((starts-with command "SELECT" :test #'equalp)
                   (incf (select-counter-of command-counter)))
                  ((starts-with command "UPDATE" :test #'equalp)
                   (incf (update-counter-of command-counter)))
                  ((starts-with command "DELETE" :test #'equalp)
                   (incf (delete-counter-of command-counter)))))))
