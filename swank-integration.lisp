;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms)

(register-readtable-for-swank
 '("CL-RDBMS" "CL-RDBMS-TEST" "CL-RDBMS.POSTGRESQL" "CL-RDBMS.ORACLE") 'setup-readtable)
