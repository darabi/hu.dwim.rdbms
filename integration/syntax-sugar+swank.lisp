;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.rdbms)

#+#.(cl:when (cl:find-package "SWANK") '(:and))
(register-readtable-for-swank
 '(:hu.dwim.rdbms :hu.dwim.rdbms.test :hu.dwim.rdbms.postgresql :hu.dwim.rdbms.oracle) 'setup-readtable)
