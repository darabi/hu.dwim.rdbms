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
   (encoding
    :utf-8
    :type (member :utf-8 :us-ascii))))

(defcondition* rdbms-error ()
  ())

(defcondition* simple-rdbms-error (simple-error)
  ())

