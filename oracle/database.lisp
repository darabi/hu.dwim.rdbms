;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms.oracle)

#.(file-header)

(defclass* oracle (database)
  ())

(defclass* oracle-transaction ()
  ())

(cffi:define-foreign-library oracle-oci
  (:unix (:or "lib?.so" "lib?32.so"))
  (:windows "lib?.dll")
  (t (:default "lib?")))
