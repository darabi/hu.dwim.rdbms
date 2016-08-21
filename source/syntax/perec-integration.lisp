;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.rdbms)

(defclass lexical-variable/rdbms ()
  ())

(def function lexical-variable-p (x)
  (typep x 'lexical-variable/rdbms))

(def constant +oid-class-id-bit-size+ 16
  "Size of the class id in bits. These are the lower bits in the oid.")

(defgeneric value->sql-literal (value type type-info &optional args))
