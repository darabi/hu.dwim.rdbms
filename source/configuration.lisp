;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.rdbms)

;;; These definitions need to be available by the time we are reading the other files, therefore
;;; they are in a standalone file.

(def function setup-readtable ()
  (enable-sharp-boolean-syntax)
  (enable-lambda-with-bang-args-syntax))
