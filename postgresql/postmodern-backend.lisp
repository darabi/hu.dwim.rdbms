;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms.postgresql)

(defclass* postgresql-postmodern-transaction (transaction)
  ((connection
    nil
    :documentation "The Postmodern connection")
   (muffle-warnings (muffle-warnings? *database*) :type boolean :accessor muffle-warnings?)))

(defmethod transaction-mixin-class list ((db postgresql-postmodern))
  'postgresql-postmodern-transaction)

(defmethod prepare-command ((db postgresql-postmodern) (tr postgresql-postmodern-transaction) (command string)
                            &key (name (generate-unique-postgresql-name "prepared_")))
  (cl-postgres:prepare-query (connection-of tr) name command)
  (make-instance 'prepared-statement :name name :query command))

(def special-variable *cl-postgres-sql-readtable* (cl-postgres:copy-sql-readtable))

(local-time:set-local-time-cl-postgres-readers *cl-postgres-sql-readtable*)

(defun execute-postmodern-prepared-statement (db connection statement-name &key binding-types binding-values visitor result-type &allow-other-keys)
  (assert (not (and visitor (not (zerop (length binding-values)))))
          (visitor binding-values) "Using a visitor and bindings at the same time is not supported by the ~A backend" db)
  (bind ((cl-postgres:*sql-readtable* *cl-postgres-sql-readtable*))
    (cl-postgres:exec-prepared
     connection statement-name
     ;; TODO could use a vector to send the bindings to cl-postgres
     (iter (for type :in-vector binding-types)
           (for value :in-vector binding-values)
           (collect (cond
                      ;; be careful when reordering stuff here! the order of these nested conds
                      ;; and etypecase'es is important.
                      ((eq value :null)
                       :null)
                      ((typep type 'sql-boolean-type)
                       (if (stringp value)
                           value
                           (if value "TRUE" "FALSE")))
                      ((eq value nil)
                       :null)
                      (t
                       (etypecase type
                         ((or sql-timestamp-type
                              sql-date-type
                              sql-time-type)
                          (if (stringp value)
                              ;; let the user talk to PostgreSQL directly using strings
                              value
                              (etypecase type
                                (sql-timestamp-type
                                  (local-time:format-rfc3339-timestring nil value :timezone local-time:+utc-zone+))
                                (sql-date-type
                                  (unless (and (zerop (local-time:sec-of value))
                                               (zerop (local-time:nsec-of value)))
                                    (cerror "continue"
                                            "Binding a local-time date that has time values which is about to be silently dropped. The binding value in question is: ~A"
                                            value))
                                  (local-time:format-rfc3339-timestring nil value :omit-time-part #t :timezone local-time:+utc-zone+))
                                (sql-time-type
                                  (unless (zerop (local-time:day-of value))
                                    (cerror "continue"
                                            "Binding a local-time time that has day value which is about to be silently dropped. The binding value in question is: ~A"
                                            value))
                                  (local-time:format-rfc3339-timestring nil value :omit-date-part #t :timezone local-time:+utc-zone+)))))
                         ((or sql-simple-type
                              sql-string-type
                              sql-float-type
                              sql-integer-type
                              sql-binary-large-object-type)
                          value))))))
     (if visitor
         (cl-postgres:row-reader (fields)
           (ecase result-type
             (vector (loop
                        with row = (make-array (length fields))
                        while (cl-postgres:next-row)
                        do (progn
                             (loop
                                for field :across fields
                                for next-field = (cl-postgres:next-field field)
                                for idx :upfrom 0
                                do (setf (aref row idx) next-field))
                             (funcall visitor row))))
             (list (loop
                      while (cl-postgres:next-row)
                      do (let ((row (loop
                                       for field :across fields
                                       collect (cl-postgres:next-field field))))
                           (funcall visitor row))))))
         (ecase result-type
           (list #'cl-postgres:list-row-reader)
           (vector #'cl-postgres:vector-row-reader))))))

(defmethod execute-command :around ((db postgresql-postmodern) (tr postgresql-postmodern-transaction) command &key &allow-other-keys)
  (if (muffle-warnings? tr)
      (handler-bind ((cl-postgres:postgresql-warning #'muffle-warning))
        (call-next-method))
      (call-next-method)))

(defmethod execute-command ((db postgresql-postmodern) (tr postgresql-postmodern-transaction) (command string)
                            &rest args)
  (let ((connection (connection-of tr))
        (statement-name "")) ; unnamed prepared statement
    (cl-postgres:prepare-query connection statement-name command)
    (handler-case
        (apply #'execute-postmodern-prepared-statement db connection statement-name args)
      (cl-postgres-error:lock-not-available (error)
        (unable-to-obtain-lock-error error)))))

(defmethod execute-command ((db postgresql-postmodern) (tr postgresql-postmodern-transaction) (prepared-statement prepared-statement)
                            &rest args)
  (apply #'execute-postmodern-prepared-statement db (connection-of tr) (name-of prepared-statement) args))

(defmethod connection-of :around ((tr postgresql-postmodern-transaction))
  (aif (call-next-method)
       it
       (let ((db (database-of tr)))
         (log.debug "Opening Postmodern connection the first time it was needed, using ~S" (remove-from-plist (connection-specification-of db) :password))
         (aprog1
             (loop
               (with-simple-restart (retry "Retry connecting")
                 (return
                   (setf (connection-of tr)
                         (apply #'cl-postgres:open-database
                                (bind (((&key (host "localhost") (port 5432) database user-name (password ""))
                                        (connection-specification-of db)))
                                  (list database user-name password host port)))))))
           (log.debug "Succesfully opened Postmodern connection ~A for transaction ~A in database ~A"
                      it tr db)))))

(defmethod cleanup-transaction :after ((tr postgresql-postmodern-transaction))
  (awhen (slot-value tr 'connection)
    (log.debug "Closing Postmodern connection ~A of transaction ~A in database ~A" it tr (database-of tr))
    (cl-postgres:close-database it)
    (setf (connection-of tr) nil)))
