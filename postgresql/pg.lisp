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

(defparameter *unique-counter* 0)

(defun generate-unique-postgresql-name (base)
  (strcat base (incf *unique-counter*)))

(defmethod execute-command ((db postgresql-pg) (tr postgresql-pg-transaction) (command string) &key visitor bindings &allow-other-keys)
  (assert (not (and visitor bindings)) (visitor bindings) "Using a visitor and bindings at the same time is not supported by the ~A backend" db)
  (let ((connection (connection-of tr))
        (portal-name nil))
    (setf command
          (loop with result = (make-array (+ (length command) 16) :element-type 'character :adjustable #t :fill-pointer 0)
                with counter = 0
                for char :across command
                do (if (char= char #\?)
                       (progn
                         (vector-push-extend #\$ result)
                         (loop for char :across (princ-to-string (incf counter)) do
                               (vector-push-extend char result)))
                       (vector-push-extend char result))
                finally (return result)))
    (unwind-protect
         (progn
           (when bindings
             (let ((statement-name (generate-unique-postgresql-name "statement")))
               (multiple-value-bind (binding-types bindings)
                   (loop for (type value) :on bindings :by #'cddr
                         ;; TODO this is hackish here: we subseq to drop any type parameters starting with #\(
                         collect (let ((str (format-sql-to-string type :database db)))
                                   (aif (position #\( str :test #'char=)
                                        (subseq str 0 it)
                                        str)) :into binding-types
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




