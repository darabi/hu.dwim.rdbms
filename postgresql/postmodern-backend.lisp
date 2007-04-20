;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms.postgresql)

#.(file-header)

(defclass* postgresql-postmodern (postgresql)
  ((muffle-warnings #f :type boolean)))

(publish-backend-symbol 'postgresql-postmodern)

(defclass* postgresql-postmodern-transaction (transaction)
  ((connection
    nil
    :documentation "The Postmodern connection")
   (muffle-warnings (muffle-warnings-p *database*) :type boolean)))

(defmethod transaction-mixin-class list ((db postgresql-postmodern))
  'postgresql-postmodern-transaction)

(defmethod prepare-command ((db postgresql-postmodern) (tr postgresql-postmodern-transaction) (command string) &key (name (generate-unique-postgresql-name)))
  (cl-postgres:prepare-query (connection-of tr) name command)
  (make-instance 'prepared-statement :name name :query command))

(defun execute-postmodern-prepared-statement (db connection statement-name bindings visitor)
  (assert (not (and visitor bindings)) (visitor bindings) "Using a visitor and bindings at the same time is not supported by the ~A backend" db)
  (cl-postgres:exec-prepared connection statement-name
                             (loop for (type value) :on bindings :by #'cddr
                                   collect (if (typep type 'sql-boolean-type)
                                               (if (stringp value)
                                                   value
                                                   (if value "TRUE" "FALSE"))
                                               (if value
                                                   (etypecase type
                                                     (sql-binary-large-object-type value)
                                                     ((or sql-simple-type
                                                          sql-string-type
                                                          sql-float-type
                                                          sql-integer-type)
                                                      (princ-to-string
                                                       (if (numberp value)
                                                           (lisp-number-to-sql-number value)
                                                           value))))
                                                   nil)))
                             (if visitor
                                 (cl-postgres:row-reader (fields)
                                   (loop with row = (make-array (length fields))
                                         while (cl-postgres:next-row)
                                         do (progn
                                              (loop for field :across fields
                                                    for next-field = (cl-postgres:next-field field)
                                                    for idx :upfrom 0
                                                    do (setf (aref row idx) (if (eq :null next-field)
                                                                                nil
                                                                                next-field)))
                                              (funcall visitor row))))
                                 'cl-postgres:list-row-reader)))

(defmethod execute-command :around ((db postgresql-postmodern) (tr postgresql-postmodern-transaction) command &key &allow-other-keys)
  (if (muffle-warnings-p tr)
      (handler-bind ((cl-postgres:postgresql-warning #'muffle-warning))
        (call-next-method))
      (call-next-method)))

(defmethod execute-command ((db postgresql-postmodern) (tr postgresql-postmodern-transaction) (command string)
                            &key visitor bindings &allow-other-keys)
  (let ((connection (connection-of tr))
        (statement-name "")) ; unnamed prepared statement
    (cl-postgres:prepare-query connection statement-name command)
    (execute-postmodern-prepared-statement db connection statement-name bindings visitor)))

(defmethod execute-command ((db postgresql-postmodern) (tr postgresql-postmodern-transaction) (prepared-statement prepared-statement)
                            &key visitor bindings &allow-other-keys)
  (execute-postmodern-prepared-statement db (connection-of tr) (name-of prepared-statement) bindings visitor))

(defmethod connection-of :around ((tr postgresql-postmodern-transaction))
  (aif (call-next-method)
       it
       (let ((db (database-of tr)))
         (log.debug "Opening Postmodern connection the first time it was needed, using ~S" (remove-keywords (connection-specification-of db) :password))
         (aprog1
             (setf (connection-of tr) (apply #'cl-postgres:open-database
                                             (destructuring-bind (&key (host "localhost") (port 5432) database user-name (password ""))
                                                 (connection-specification-of db)
                                               (list database user-name password host port))))
           (log.debug "Succesfully opened Postmodern connection ~A for transaction ~A in database ~A"
                      it tr db)))))

(defmethod cleanup-transaction ((tr postgresql-postmodern-transaction))
  (awhen (slot-value tr 'connection)
    (log.debug "Closing Postmodern connection ~A of transaction ~A in database ~A" it tr (database-of tr))
    (cl-postgres:close-database it)
    (setf (connection-of tr) nil)))



