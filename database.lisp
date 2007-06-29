;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms)

#.(file-header)

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

(defcondition* simple-rdbms-error (simple-error)
  ())

(defun simple-rdbms-error (message &rest args)
  (error 'simple-rdbms-error :format-control message :format-arguments args))

(defmethod shared-initialize :after ((database database) slot-names
                                     &key transaction-mixin generated-transaction-class-name &allow-other-keys)
  (let ((classes (mapcar #'find-class (transaction-mixin-class database))))
    (setf (transaction-class-of database)
          (make-instance 'standard-class
                         :name generated-transaction-class-name
                         :direct-superclasses (aif transaction-mixin
                                                   (cons (find-class it) classes)
                                                   classes)))))

(defgeneric transaction-mixin-class (database)
  (:documentation "Collects the transaction mixin classes which will be inherited by the transaction class instantiated by with-transaction.")

  (:method-combination list))

(defmacro with-database (database &body body)
  `(let ((*database* ,database))
    ,@body))

(defgeneric calculate-rdbms-name (database thing name)
  (:documentation "May be specialized to take name length and character set limitations into account.")
  (:method ((database database) thing name)
           (string-downcase name)))

(defun rdbms-name-for (name &optional thing)
  (declare (cl:type (or null (member :table :index :column :sequence)) thing))
  (calculate-rdbms-name *database* thing name))

(defun calculate-rdbms-name-with-utf-8-length-limit (name limit)
  "Cuts off the end of names that are too long and appends the SXHASH of the original name."
  (assert (>= limit 8))
  (let ((name-as-string (string-downcase name)))
    (iter (for char :in-sequence "*\\/-")
          (nsubstitute #\_ char name-as-string :test #'char=))
    (let ((name-as-bytes (string-to-octets name-as-string :utf-8)))
      (when (> (length name-as-bytes)
               limit)
        (let ((hash (logand (sxhash name-as-string) #.(1- (expt 2 32)))))
          (iter (while (> (length name-as-bytes)
                          (- limit 8)))
                (setf name-as-string (subseq name-as-string 0 (1- (length name-as-string))))
                (setf name-as-bytes (string-to-octets name-as-string :utf-8)))
          (setf name-as-string
                (strcat name-as-string (format nil "~8,'0X" hash)))))
      name-as-string)))