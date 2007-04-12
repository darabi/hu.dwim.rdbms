;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms.postgresql)

#.(file-header)

(defclass* postgresql-postmodern (postgresql)
  ())

(publish-backend-symbol 'postgresql-postmodern)

(defclass* postgresql-postmodern-transaction (transaction)
  ((connection
    nil
    :documentation "The Postmodern connection")))

(defmethod transaction-class-name list ((db postgresql-postmodern))
  'postgresql-postmodern-transaction)

(defmethod execute-command ((db postgresql-postmodern) (tr postgresql-postmodern-transaction) (command string) &key visitor bindings &allow-other-keys)
  (assert (not (and visitor bindings)) (visitor bindings) "Using a visitor and bindings at the same time is not supported by the ~A backend" db)
  (assert (not bindings) (bindings) "Bindings are not supported by the ~A backend" db)
  (cl-postgres:exec-query (connection-of tr) command (or visitor 'cl-postgres:list-row-reader)))

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




