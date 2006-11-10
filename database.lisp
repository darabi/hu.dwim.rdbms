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
    :documentation "Backend specific connection data, usually a plist of args passed to the connect function.")))

;; TODO get rid of it?
#+nil(defgeneric transaction-mixins (database)
  (:documentation "Returns a list of the transaction mixins that will form the final transaction class.")
  (:method-combination list)
  (:method :list ((db database))
           'transaction))




