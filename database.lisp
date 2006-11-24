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
   (transaction-mixin
    nil
    :type symbol)
   (transaction-class
    :type standard-class)
   (encoding
    :utf-8
    :type (member :utf-8 :us-ascii))))

(defcondition* rdbms-error ()
  ())

(defcondition* simple-rdbms-error (simple-error)
  ())

(defmethod shared-initialize :after ((database database) slot-names &key &allow-other-keys)
  (let ((classes (mapcar #'find-class (transaction-class-name database))))
    (setf (transaction-class-of database)
          (make-instance 'standard-class
                         :direct-superclasses (aif (transaction-mixin-of database)
                                                   (cons (find-class it) classes)
                                                   classes)))))

(defgeneric transaction-class-name (database)
  (:method-combination list))

(defmacro with-database (database &body body)
  `(let ((*database* ,database))
    ,@body))
