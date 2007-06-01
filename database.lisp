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
