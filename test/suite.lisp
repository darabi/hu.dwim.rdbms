;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.rdbms.test)

(def function setup-readtable ()
  (enable-sharp-boolean-syntax)
  (enable-sharp-l-syntax)
  (enable-sql-syntax)
  (enable-string-quote-syntax))

#+#.(cl:when (cl:find-package "SWANK") '(:and))
(register-readtable-for-swank
 '(:hu.dwim.rdbms.test) 'setup-readtable)

(def suite* (test :in root-suite))
