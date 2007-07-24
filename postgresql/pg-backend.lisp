;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms.postgresql)

#.(file-header)

(defmethod initialize-instance :after ((database postgresql-pg) &key encoding &allow-other-keys)
  ;; trigger our custom accessor below
  (setf (encoding-of database) (or encoding (encoding-of database))))

(defmethod (setf encoding-of) :around (new-value (database postgresql-pg))
  (setf (native-encoding-of database)
        (case new-value
          ((or :utf8 :utf-8) "UTF8")
          ((or :us-ascii :ascii :latin1 :iso-8859-1) "LATIN1")
          ((or :latin2 :iso-8859-2) "LATIN2")
          (t (simple-rdbms-error "Unsupported encoding ~S" new-value))))
  (call-next-method))

(defclass* postgresql-pg-transaction (transaction)
  ((connection
    nil
    :documentation "The pg connection retuned by")))

(defprint-object (self postgresql-pg-transaction)
  (princ ":begin-executed-p ")
  (princ (if (begin-was-executed-p self)
             "#t" "#f")))

(defmethod transaction-mixin-class list ((db postgresql-pg))
  'postgresql-pg-transaction)

(defmethod execute-command ((db postgresql-pg) (tr postgresql-pg-transaction) (command string) &key visitor bindings &allow-other-keys)
  (assert (not (and visitor bindings)) (visitor bindings) "Using a visitor and bindings at the same time is not supported by the ~A backend" db)
  (let ((connection (connection-of tr))
        (portal-name nil))
    (unwind-protect
         (progn
           (when bindings
             (let ((statement-name (generate-unique-postgresql-name "statement")))
               (multiple-value-bind (binding-types bindings)
                   (loop for (type value) :on bindings :by #'cddr
                         ;; TODO this is hackish here: we subseq to drop any type parameters starting with #\(
                         collect (internal-type-for-sql-type type) :into binding-types
                         collect (list (binding-type-for-sql-type type db) value) :into bindings
                         finally (return (values binding-types bindings)))
                 (setf portal-name (generate-unique-postgresql-name "portal"))
                 (pg:pg-prepare connection statement-name command binding-types)
                 (pg:pg-bind connection portal-name statement-name bindings))))
           (if visitor
               (let ((cursor-name (generate-unique-postgresql-name "cursor")))
                 (pg:pg-exec connection "DECLARE " cursor-name " CURSOR FOR " command)
                 (unwind-protect
                      (loop :for result = (pg:pg-result (pg:pg-exec connection "FETCH 1 FROM " cursor-name) :tuples)
                            :until (zerop (length result))
                            :do (funcall visitor (first result)))
                   (pg:pg-exec connection "CLOSE " cursor-name)))
               (pg::pgresult-tuples (if portal-name
                                        (pg:pg-execute connection portal-name)
                                        (pg:pg-exec connection command)))))
      (when portal-name
        (pg:pg-close-portal connection portal-name)))))

(defmethod connection-of :around ((tr postgresql-pg-transaction))
  (aif (call-next-method)
       it
       (let ((db (database-of tr)))
         (log.debug "Opening pg connection the first time it was needed, using ~S" (remove-keywords (connection-specification-of db) :password))
         (aprog1
             (setf (connection-of tr) (apply #'pg::pg-connect/v3
                                             (destructuring-bind (&key (host "localhost") (port 5432) database user-name (password ""))
                                                 (connection-specification-of db)
                                               (list database user-name :password password :host host :port port
                                                     :encoding (native-encoding-of db)))))
           (log.debug "Succesfully opened pg connection ~A for transaction ~A in database ~A with encoding ~S"
                      it tr db (native-encoding-of db))))))

(defmethod cleanup-transaction ((tr postgresql-pg-transaction))
  (awhen (slot-value tr 'connection)
    (log.debug "Closing pg connection ~A of transaction ~A in database ~A" it tr (database-of tr))
    (pg::pg-disconnect it)
    (setf (connection-of tr) nil)))

(macrolet ((def (&body defs)
             `(progn
               ,@(loop for def :in defs
                       collect (destructuring-bind (arg &body body) def
                                 `(defmethod binding-type-for-sql-type (,arg (database postgresql-pg)) ,@body))))))
  (def
    ((type sql-boolean-type)
     :boolean)

    ((type sql-integer-type)
     (let ((bit-size (bit-size-of type)))
       (cond ((null bit-size)
              :numeric)
             ((<= bit-size 16)
              :int16)
             ((<= bit-size 32)
              :int32)
             ((<= bit-size 64)
              :int64)
             (t
              :numeric))))

    ((type sql-float-type)
     :float)
  
    ((type sql-numeric-type)
     :numeric)
  
    ((type sql-string-type)
     :string)

    ((type sql-date-type)
     :string)

    ((type sql-timestamp-type)
     :string)

    ((type sql-time-type)
     :string)

    ((type sql-binary-large-object-type)
     :rawdata)))
