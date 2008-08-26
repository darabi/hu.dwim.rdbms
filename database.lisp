;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms)

;;;;;;;;;;;;;
;;; Databases

(defvar *database*)

(defclass* database ()
  ((connection-specification
    :documentation "Backend specific connection data, usually a plist of args passed to the connect function.")
   (default-result-type
    'vector
    :type (member vector list cursor))
   (transaction-class
    :type standard-class
    :documentation "Transactions will be instances of this class. This class is created according to the generic method transaction-mixin-class.")
   (encoding
    :utf-8
    :type (member :utf-8 :us-ascii))))

(defcondition* rdbms-error ()
  ())

(defcondition* translated-rdbms-error (rdbms-error)
  ((original-error)))

(defcondition* simple-rdbms-error (simple-error)
  ())

(defun simple-rdbms-error (message &rest args)
  (error 'simple-rdbms-error :format-control message :format-arguments args))

(defcondition* unable-to-obtain-lock-error (translated-rdbms-error simple-rdbms-error)
  ())

(defun unable-to-obtain-lock-error (message-or-nested-condition)
  (error 'unable-to-obtain-lock-error
         :format-control (princ-to-string message-or-nested-condition)
         :original-error (when (typep message-or-nested-condition 'condition)
                           message-or-nested-condition)))

(defmethod shared-initialize :after ((database database) slot-names
                                     &key transaction-mixin generated-transaction-class-name &allow-other-keys)
  (let ((classes (mapcar #'find-class (transaction-mixin-class database))))
    (setf (transaction-class-of database)
          (make-instance 'standard-class
                         :name generated-transaction-class-name
                         :direct-superclasses (aif transaction-mixin
                                                   (cons (find-class it) classes)
                                                   classes)))))

(def (generic e) transaction-mixin-class (database)
  (:documentation "Collects the transaction mixin classes which will be inherited by the transaction class instantiated by with-transaction when using this database.")

  (:method-combination list))

(def (macro e) with-database (database &body body)
  `(let ((*database* ,database))
    ,@body))

;;;;;;;;;;;;;;;
;;; RDBMS names

(defgeneric calculate-rdbms-name (database thing name)
  (:documentation "May be specialized to take name length and character set limitations into account.")
  (:method ((database database) thing name)
           (string-downcase name)))

(defun rdbms-name-for (name &optional thing)
  (declare (cl:type (or null (member :table :view :index :column :sequence)) thing))
  (calculate-rdbms-name *database* thing name))

(defun calculate-rdbms-name-with-utf-8-length-limit (name limit &key prefix)
  "Cuts off the end of names that are too long and appends the hash of the original name."
  (assert (>= limit 8))
  (let ((name-as-string (concatenate-string prefix (string-downcase name))))
    (iter (for char :in-sequence "*\\/-~%")
          (nsubstitute #\_ char name-as-string :test #'char=))
    (let ((name-as-bytes (string-to-octets name-as-string :encoding :utf-8)))
      (when (> (length name-as-bytes)
               limit)
        (let ((hash
               (ironclad:byte-array-to-hex-string
                (ironclad:digest-sequence :crc32 name-as-bytes))))
          (iter (while (> (length name-as-bytes)
                          (- limit 8)))
                (setf name-as-string (subseq name-as-string 0 (1- (length name-as-string))))
                (setf name-as-bytes (string-to-octets name-as-string :encoding :utf-8)))
          (setf name-as-string
                (concatenate-string name-as-string (format nil "~8,'0X" hash)))))
      name-as-string)))

;;;;;;;;;;;;;;;;;;;
;;; Oracle database

(defclass* oracle (database)
  ((connection-encoding
    :utf-16
    :type (member :ascii :utf-16))))

(let ((loaded-p #f))
  (defmethod initialize-instance :before ((self oracle) &key &allow-other-keys)
    (unless loaded-p
      (asdf:operate 'asdf:load-op :cl-rdbms.oracle)
      (eval (read-from-string
             ;; TODO let the user control version, path and stuff through initargs
             "(let ((cffi:*foreign-library-directories*
                     (list #P\"/usr/lib/oracle/xe/app/oracle/product/10.2.0/client/lib/\")))
                (cffi:load-foreign-library 'cl-rdbms.oracle::oracle-oci))"))
      (setf loaded-p #t))))

;;;;;;;;;;;;;;;;;;;;;;;
;;; Postgresql database

(defclass* postgresql (database)
  ())

(defclass* postgresql-postmodern (postgresql)
  ((muffle-warnings #f :type boolean :accessor muffle-warnings?)))

(let ((loaded-p #f))
  (defmethod initialize-instance :before ((self postgresql-postmodern) &key &allow-other-keys)
    (unless loaded-p
      (asdf:operate 'asdf:load-op :cl-rdbms.postmodern)
      (setf loaded-p #t))))

;;;;;;;;;;;;;;;;;;;
;;; Sqlite database

(defclass* sqlite (database)
  ())

(let ((loaded-p #f))
  (defmethod initialize-instance :before ((self sqlite) &key &allow-other-keys)
    (unless loaded-p
      (asdf:operate 'asdf:load-op :cl-rdbms.sqlite)
      (eval (read-from-string
             "(cffi:load-foreign-library 'cl-rdbms.sqlite::sqlite3)"))
      (setf loaded-p #t))))
