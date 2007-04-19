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

(defmacro with-transaction (&body body)
  `(with-transaction* ()
    ,@body))

(defmacro with-transaction* ((&rest args &key database (default-terminal-action :commit) &allow-other-keys)
                             &body body)
  (remf-keywords args :database :default-terminal-action)
  (with-unique-names (body-finished-p)
    `(let* (,@(when database `((*database* ,database)))
            (*transaction* nil)
            (,body-finished-p #f))
      (unless (boundp '*database*)
        (error "Cannot start transaction because database was not provided, either use with-database or provide a database to with-transaction"))
      (unwind-protect
           (progn
             (begin :terminal-action ,default-terminal-action ,@args)
             (multiple-value-prog1
                 (progn
                   ,@body)
               (setf ,body-finished-p #t)
               (ecase (terminal-action-of *transaction*)
                 ((:commit :marked-for-commit-only)
                  (commit))
                 ((:rollback :marked-for-rollback-only)
                  (rollback)))))
        (unless ,body-finished-p
          (rollback))
        (when *transaction*
          (cleanup-transaction *transaction*))))))

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
      (setf (timestamp-of *transaction*) (caar (execute "select now()")))))

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
  (setf *transaction* (apply #'make-transaction *database* args)))

(defun commit ()
  (assert-transaction-in-progress)
  (commit-transaction *database* *transaction*))

(defun rollback ()
  (assert-transaction-in-progress)
  (rollback-transaction *database* *transaction*))

(defun execute (command &rest args &key visitor bindings (with-transaction #f) &allow-other-keys)
  (declare (ignore visitor bindings))   ; for slime to bring up the arguments
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
  (apply #'execute command :with-transaction :ensure args))

(defmethod transaction-mixin-class list (database)
  'transaction)
  
(defgeneric make-transaction (database &key &allow-other-keys)
  (:method :before (database &key &allow-other-keys)
           (log.debug "About to BEGIN transaction in database ~A" database))

  (:method (database &rest args)
           (apply #'make-instance (transaction-class-of database) :database database :state :in-progress args)))

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

(defgeneric execute-command (database transaction command &key visitor bindings &allow-other-keys)
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
                                                  for el in (cdr bindings) by #'cddr
                                                  collect (strcat (format nil "$~A = " i) (format-sql-to-string el))))))
             (sql-log.info "; ~A" command)))

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


(defclass* transaction-with-commit-hooks-mixin ()
  ((after-commit-hooks '() :type list)))

(defcondition* commit-hook-execution-error (transaction-error)
  ((condition)))

(defmethod commit-transaction :after (database (transaction transaction-with-commit-hooks-mixin))
  (loop for hook :in (after-commit-hooks-of transaction) do
        (block calling
          (handler-bind
              ((serious-condition (lambda (c)
                                    (cerror "Continue" 'commit-hook-execution-error :condition c)
                                    (return-from calling))))
            (funcall hook)))))

(defun register-commit-hook (hook &key (type :after) &allow-other-keys)
  (register-commit-hook-in-transaction *transaction* hook :type type))

(defgeneric register-commit-hook-in-transaction (transaction hook &key type &allow-other-keys)
  (:method ((transaction transaction-with-commit-hooks-mixin) (hook function) &key (type :after) &allow-other-keys)
           (assert (eq type :after) () "Only :after commit hooks are supported for now")
           (push hook (after-commit-hooks-of transaction))))

