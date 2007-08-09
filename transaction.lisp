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

(defclass* prepared-statement ()
  ((name)
   (query :documentation "The query passed in when this statement was prepared.")))

(defclass* transaction ()
  ((database
    :type database)
   (default-result-type
    :type (member vector list cursor))
   (timestamp
    nil
    :type integer)
   (command-counter
    (make-instance 'command-counter)
    :type command-counter)
   (begin-was-executed
    #f
    :type boolean)
   (state
    :uninitialized
    :type (member :uninitialized :committed :rolled-back :in-progress))
   (terminal-action
    :commit
    :type (member :commit :rollback :marked-for-commit-only :marked-for-rollback-only)
    :documentation "Used by with-transaction to decide what to do when the with-transaction body finishes without any errors."))
  (:documentation "An object representing a transaction context. The actual backend connection/transaction is usually lazily created."))

(defprint-object (self transaction)
  (princ ":begin-executed-p ")
  (princ (if (begin-was-executed-p self)
             "#t" "#f")))

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

(defun current-insert-counter ()
  (insert-counter-of (command-counter-of *transaction*)))

(defun current-select-counter ()
  (select-counter-of (command-counter-of *transaction*)))

(defun current-update-counter ()
  (update-counter-of (command-counter-of *transaction*)))

(defun current-delete-counter ()
  (delete-counter-of (command-counter-of *transaction*)))

(defmacro with-transaction (&body forms)
  `(with-transaction* ()
    ,@forms))

(defmacro with-transaction* ((&rest args &key database (default-terminal-action :commit) &allow-other-keys)
                             &body forms)
  (declare (ignore database default-terminal-action))
  `(funcall-with-transaction
    (lambda ()
      ,@forms)
    ,@args))

(defun funcall-with-transaction (function &rest args &key (default-terminal-action :commit) database &allow-other-keys)
  (unless (or database (boundp '*database*))
    (error "Cannot start transaction because database was not provided, either use with-database or provide a database to with-transaction*"))
  (let* ((*database* (or database *database*))
         (*transaction* nil)
         (body-finished-p #f))
    (unwind-protect
         (progn
           (setf *transaction*
                 (apply #'make-transaction *database*
                        :terminal-action default-terminal-action
                        (remf-keywords args :database :default-terminal-action)))
           (multiple-value-prog1
               (with-simple-restart (exit-transaction "Terminate the transaction with the terminal action ~S" (terminal-action-of *transaction*))
                 (funcall function))
             (setf body-finished-p #t)
             (ecase (terminal-action-of *transaction*)
               ((:commit :marked-for-commit-only)
                (commit-transaction *database* *transaction*))
               ((:rollback :marked-for-rollback-only)
                (rollback-transaction *database* *transaction*)))))
      (when *transaction*
        (unless body-finished-p
          (handler-case
              (rollback-transaction *database* *transaction*)
            (serious-condition (condition)
                               (log.warn "Ignoring error while trying to rollback transaction in a failed with-transaction block: ~A" condition))))
        (cleanup-transaction *transaction*)))))

(defmethod (setf terminal-action-of) :before (new-value (transaction transaction))
  (when (and (member (terminal-action-of *transaction*) '(:marked-for-rollback-only :marked-for-commit-only))
             (not (eq new-value (terminal-action-of *transaction*))))
    (error "You can not set the terminal action of the transaction ~A from ~A to ~A"
           *transaction* (terminal-action-of *transaction*) new-value)))

(defun mark-transaction-for-commit-only ()
  (assert-transaction-in-progress)
  (setf (terminal-action-of *transaction*) :marked-for-commit-only))

(defun mark-transaction-for-rollback-only ()
  (assert-transaction-in-progress)
  (setf (terminal-action-of *transaction*) :marked-for-rollback-only))

(defun transaction-timestamp ()
  (or (timestamp-of *transaction*)
      (setf (timestamp-of *transaction*) (first* (first* (execute "select now()"))))))

(defun in-transaction-p ()
  (and (boundp '*transaction*)
       *transaction*
       (transaction-in-progress-p *transaction*)))

(defun transaction-in-progress-p (&optional (transaction *transaction*))
  (eq (state-of transaction) :in-progress))

(defun transaction-valid-p (&optional (transaction *transaction*))
  "Returns true if we have a running transaction and its terminal action will be a commit."
  (and (transaction-in-progress-p transaction)
       (not (eq (terminal-action-of transaction) :marked-for-rollback-only))))

(defun assert-transaction-in-progress ()
  (unless (in-transaction-p)
    (error 'transaction-error :format-control "No transaction in progress")))

(defun begin (&rest args)
  "Starts a new transaction. This transaction must be closed by an explicit call to either rollback or commit. See with-transaction for convenience and safety. This is for debug purposes."
  (assert (not (boundp '*transaction*)))
  (setf *transaction* (apply #'make-transaction *database* args)))

(defun commit ()
  "Commits the current transaction. The transaction must be started by an explicit call to begin. This is for debug purposes."
  (assert-transaction-in-progress)
  (unwind-protect
       (commit-transaction *database* *transaction*)
    (cleanup-transaction *transaction*)
    (makunbound '*transaction*))
  (values))

(defun rollback ()
  "Rolls back the current transaction. The transaction must be started by an explicit call to begin. This is for debug purposes."
  (assert-transaction-in-progress)
  (unwind-protect
       (rollback-transaction *database* *transaction*)
    (cleanup-transaction *transaction*)
    (makunbound '*transaction*))
  (values))

(defun execute (command &rest args &key visitor bindings result-type (with-transaction #f) &allow-other-keys)
  "Executes an SQL command. If VISITOR is not present the result is returned in a sequence. The type of the sequence is determined by RESULT-TYPE which is either LIST or VECTOR. When VISITOR is present it will be called for each row in the result."
  (declare (ignore visitor bindings result-type))   ; for slime to bring up the arguments
  (flet ((%execute-command ()
           (apply 'execute-command *database* *transaction* command args)))
    (if (or (eq :new with-transaction)
            (and (not (in-transaction-p))
                 (eq :ensure with-transaction)))
        (with-transaction
          (%execute-command))
        (progn
          (assert-transaction-in-progress)
          (%execute-command)))))

(defun execute-ddl (command &rest args &key &allow-other-keys)
  "A DDL statement is executed in a separate transaction."
  (apply #'execute command :with-transaction :ensure args))

(defmethod transaction-mixin-class list (database)
  'transaction)
  
(defgeneric make-transaction (database &key &allow-other-keys)
  (:method :before (database &key &allow-other-keys)
           (log.debug "About to BEGIN transaction in database ~A" database))

  (:method (database &rest args &key default-result-type &allow-other-keys)
           (apply #'make-instance (transaction-class-of database)
                  :database database
                  :state :in-progress
                  :default-result-type (or default-result-type (default-result-type-of database))
                  args)))

(defgeneric begin-transaction (database transaction)
  (:method (database transaction)
           (execute-command database transaction "BEGIN")))

(defgeneric commit-transaction (database transaction)
  (:method :around (database transaction)
           (log.debug "About to COMMIT transaction ~A" transaction)
           (when (begin-was-executed-p transaction)
             (call-next-method))
           (setf (state-of transaction) :committed)
           (values))

  (:method (database transaction)
           (execute-command database transaction "COMMIT")))

(defgeneric rollback-transaction (database transaction)
  (:method :around (database transaction)
           (log.dribble "About to ROLLBACK transaction ~A" transaction)
           (when (begin-was-executed-p transaction)
             (call-next-method))
           (setf (state-of transaction) :rolled-back)
           (values))
  
  (:method (database transaction)
           (execute-command database transaction "ROLLBACK")))

(defgeneric cleanup-transaction (transaction))

(defgeneric prepare-command (database transaction command &key name)
  (:documentation "Sends a query to the database for parsing and returns a handler (a prepared-statement CLOS object) that can be used as a command for EXECUTE-COMMAND."))

(defgeneric execute-command (database transaction command &key visitor bindings result-type &allow-other-keys)
  (:method :before (database transaction command &key bindings &allow-other-keys)
           (unless (begin-was-executed-p transaction)
             (setf (begin-was-executed-p transaction) #t)
             (begin-transaction database transaction))
           (log.dribble "*** ~S in transaction ~A of database ~A"
                        command transaction database)
           (when (stringp command)
             (when bindings
               (sql-log.info "; ~A" (format nil "~{~A~^, ~}"
                                            (loop for i upfrom 1
                                                  for (type el) on bindings by #'cddr
                                                  collect (format nil "$~A = ~A as ~A" i el (format-sql-to-string type))))))
             (sql-log.info "; ~A" command)))

  (:method :around (database transaction command &rest args &key (result-type (default-result-type-of transaction)) &allow-other-keys)
           (apply #'call-next-method database transaction command :result-type result-type args))

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


(defclass* transaction-with-hooks-mixin ()
  ((hooks nil :type list)))

(defclass* transaction-hook ()
  ((function :type (or symbol function))
   (when :type (member :before :after))
   (action :type (member :commit :rollback :always))))

(defun funcall-transaction-hooks (transaction when action)
  (loop for hook :in (hooks-of transaction) do
        (let ((hook-action (action-of hook)))
          (when (and (eq when (when-of hook))
                     (or (eq :always hook-action)
                         (eq action hook-action)))
            (funcall (function-of hook))))))

(defmethod commit-transaction :around (database (transaction transaction-with-hooks-mixin))
  ;; this must be an around method because the default around does not
  ;; do call-next-method when begin-transaction was not executed
  (prog2 (funcall-transaction-hooks transaction :before :commit)
      (call-next-method)
    (funcall-transaction-hooks transaction :after :commit)))

(defmethod rollback-transaction :around (database (transaction transaction-with-hooks-mixin))
  ;; this must be an around method because the default around does not
  ;; do call-next-method when begin-transaction was not executed
  (prog2 (funcall-transaction-hooks transaction :before :rollback)
      (call-next-method)
    (funcall-transaction-hooks transaction :after :rollback)))

(defun register-transaction-hook (when action function)
  (register-hook-in-transaction *transaction* when action function))

(defgeneric register-hook-in-transaction (transaction when action function)
  (:method ((transaction transaction-with-hooks-mixin) when action (function function))
           (prog1-bind hook
               (make-instance 'transaction-hook
                              :function function
                              :when when
                              :action action)
             (push hook (hooks-of transaction)))))
