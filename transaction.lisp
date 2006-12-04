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
   (ddl-only
    #f
    :type boolean)
   (transaction-begin-executed
    #f
    :type boolean)
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
    `(let* (,@(when database `((*database* ,database)))
            (*transaction* nil)
            (,body-finished-p #f))
      (unwind-protect
           (progn
             (begin)
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
  (setf *transaction* (make-transaction *database*)))

(defun commit ()
  (assert-transaction-in-progress)
  (commit-transaction *database* *transaction*))

(defun rollback ()
  (assert-transaction-in-progress)
  (rollback-transaction *database* *transaction*))

(defun execute (command &rest args &key visitor bindings &allow-other-keys)
  (declare (ignore visitor bindings)) ; for slime to bring up the arguments
  (assert-transaction-in-progress)
  (apply 'execute-command *database* *transaction* command args))

(defun execute-ddl (command)
  (with-transaction
    (setf (ddl-only-p *transaction*) #t)
    (execute command)))

(defmethod transaction-class-name list (database)
  'transaction)
  
(defgeneric make-transaction (database)
  (:method :before (database)
           (log.debug "About to BEGIN transaction in database ~A" database))

  (:method (database)
           (make-instance (transaction-class-of database) :database database :state :in-progress)))

(defgeneric begin-transaction (database transaction)
  (:method (database transaction)
           (execute-command database transaction "BEGIN")))

(defgeneric commit-transaction (database transaction)
  (:method :around (database transaction)
           (log.debug "About to COMMIT transaction ~A" transaction)
           (when (transaction-begin-executed-p transaction)
             (call-next-method))
           (setf (state-of transaction) :committed)
           (values))

  (:method (database transaction)
           (execute-command database transaction "COMMIT")))

(defgeneric rollback-transaction (database transaction)
  (:method :around (database transaction)
           (log.dribble "About to ROLLBACK transaction ~A" transaction)
           (when (transaction-begin-executed-p transaction)
             (call-next-method))
           (setf (state-of transaction) :rolled-back)
           (values))
  
  (:method (database transaction)
           (execute-command database transaction "ROLLBACK")))

(defgeneric cleanup-transaction (transaction))

(defgeneric execute-command (database transaction command &key visitor bindings &allow-other-keys)
  (:method :before (database transaction command &key &allow-other-keys)
           (unless (transaction-begin-executed-p transaction)
             (setf (transaction-begin-executed-p transaction) #t)
             (begin-transaction database transaction))
           (log.dribble "*** ~S in transaction ~A of database ~A"
                        command transaction database)
           (sql-log.info "; ~A" command))

  (:method :after (database transaction (command string) &key &allow-other-keys)
           (let ((command-counter (command-counter-of transaction)))
             (cond ((starts-with command "INSERT" :test #'equalp)
                   (incf (insert-counter-of command-counter)))
                  ((starts-with command "SELECT" :test #'equalp)
                   (incf (select-counter-of command-counter)))
                  ((starts-with command "UPDATE" :test #'equalp)
                   (incf (update-counter-of command-counter)))
                  ((starts-with command "DELETE" :test #'equalp)
                   (incf (delete-counter-of command-counter)))))))
