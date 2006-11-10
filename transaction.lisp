;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms)

#.(file-header)

(defclass* transaction ()
  ()
  (:documentation "An object representing a transaction context. The actual backend connection/transaction is lazily created."))

(defmacro with-transaction (&body body)
  `(with-transaction* ()
    ,@body))

(defmacro with-transaction* (() &body body)
  `(progn
    ;; TODO
    ,@body))


(defun begin ()
  )
(defun commit ()
  )
(defun rollback ()
  )

(defgeneric begin* (database))
(defgeneric commit* (database))
(defgeneric rollback* (database))

