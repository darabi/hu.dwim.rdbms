;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms)

#.(file-header)

(defclass* postgresql-pg (postgresql)
  ())

(defclass* postgresql-pg-transaction (transaction)
  ((connection
    nil
    :reader nil
    :writer (setf connection-of)
    :documentation "The pg connection retuned by")))

(defmethod begin-transaction ((db postgresql-pg))
  (make-instance 'postgresql-pg-transaction))

(defmethod commit-transaction ((db postgresql-pg) (tr postgresql-pg-transaction))
  (execute-command db tr "COMMIT"))

(defmethod rollback-transaction ((db postgresql-pg) (tr postgresql-pg-transaction))
  (execute-command db tr "ROLLBACK"))

(defmethod execute-command ((db postgresql-pg) (tr postgresql-pg-transaction) command)
  (pg::pg-exec (connection-of tr) command))

(defmethod transaction-connected-p ((tr postgresql-pg-transaction))
  (not (null (slot-value tr 'connection))))

(defmethod connection-of ((tr postgresql-pg-transaction))
  (aif (slot-value tr 'connection)
       it
       (let ((db (database-of tr)))
         (log.debug "Opening connection the first time it was needed using ~S" (connection-specification-of db))
         (aprog1
             (setf (connection-of tr) (apply #'pg::pg-connect/v3
                                             (destructuring-bind (&key (host "localhost") (port 5432) database user-name (password ""))
                                                 (connection-specification-of db)
                                               (list database user-name :password password :host host :port port))))
           (log.debug "Succesfully opened connection ~A for transaction ~A in database ~A" it tr db)))))

(defmethod cleanup-transaction ((tr postgresql-pg-transaction))
  (awhen (slot-value tr 'connection)
    (log.debug "Closing connection ~A of transaction ~A in database ~A" it tr (database-of tr))
    (pg::pg-disconnect it)
    (setf (connection-of tr) nil)))




