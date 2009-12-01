;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.rdbms.test
  :class hu.dwim.test-system
  :depends-on (:hu.dwim.rdbms
               :hu.dwim.stefil+hu.dwim.def+swank)
  :components ((:module "test"
                :components (#+nil(:file "basic" :depends-on ("suite")) ; split to database specific and generic
                             (:file "package")
                             (:file "suite" :depends-on ("package"))
                             #+nil(:file "syntax" :depends-on ("suite")) ; split to database specific and generic
                             (:file "type" :depends-on ("suite"))))))
