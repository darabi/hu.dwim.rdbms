;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms)

#.(file-header)

(defclass* postgresql-pg (postgresql)
  ((native-encoding
    :type string)))

(defmethod initialize-instance :after ((database postgresql-pg) &key encoding &allow-other-keys)
  ;; trigger our custom accessor below
  (setf (encoding-of database) (or encoding (encoding-of database))))

(defmethod (setf encoding-of) :around (new-value (database postgresql-pg))
  (setf (native-encoding-of database)
        (case new-value
          ((or :utf8 :utf-8) "UTF8")
          ((or :us-ascii :ascii :latin1 :iso-8859-1) "LATIN1")
          ((or :latin2 :iso-8859-2) "LATIN2")
          (t (error 'simple-rdbms-error :format-control "Unsupported encoding ~S" :format-arguments (list new-value)))))
  (call-next-method))

(defclass* postgresql-pg-transaction (transaction)
  ((connection
    nil
    :reader nil
    :writer (setf connection-of)
    :documentation "The pg connection retuned by")))

(defprint-object (self postgresql-pg-transaction)
  (princ ":begin-executed-p ")
  (princ (if (transaction-begin-executed-p self)
             "#t" "#f")))

(defmethod transaction-class-name list ((db postgresql-pg))
  'postgresql-pg-transaction)

(defmethod execute-command ((db postgresql-pg) (tr postgresql-pg-transaction) command &key visitor bindings &allow-other-keys)
  (if visitor
      (pg:pg-for-each (connection-of tr) command visitor)
      (pg::pgresult-tuples (pg:pg-exec (connection-of tr) command))))

(defgeneric connection-of (tr)
  (:method ((tr postgresql-pg-transaction))
           (aif (slot-value tr 'connection)
                it
                (let ((db (database-of tr)))
                  (log.debug "Opening connection the first time it was needed, using ~S" (remove-keywords (connection-specification-of db) :password))
                  (aprog1
                      (setf (connection-of tr) (apply #'pg::pg-connect/v3
                                                      (destructuring-bind (&key (host "localhost") (port 5432) database user-name (password ""))
                                                          (connection-specification-of db)
                                                        (list database user-name :password password :host host :port port
                                                              :encoding (native-encoding-of db)))))
                    (log.debug "Succesfully opened connection ~A for transaction ~A in database ~A with encoding ~S"
                               it tr db (native-encoding-of db)))))))

(defmethod cleanup-transaction ((tr postgresql-pg-transaction))
  (awhen (slot-value tr 'connection)
    (log.debug "Closing connection ~A of transaction ~A in database ~A" it tr (database-of tr))
    (pg::pg-disconnect it)
    (setf (connection-of tr) nil)))




