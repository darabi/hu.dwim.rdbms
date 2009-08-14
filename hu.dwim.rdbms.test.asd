;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :asdf)

(load-system :hu.dwim.asdf)

(defsystem :hu.dwim.rdbms.test
  :class hu.dwim.test-system
  :author ("Attila Lendvai <attila.lendvai@gmail.com>"
           "Levente Mészáros <levente.meszaros@gmail.com>"
           "Tamás Borbély <tomi.borbely@gmail.com>")
  :licence "BSD / Public domain"
  :description "Test suite for hu.dwim.rdbms"
  :depends-on (:hu.dwim.def+hu.dwim.stefil
               :hu.dwim.rdbms)
  :components ((:module "test"
                :components (#+nil(:file "basic" :depends-on ("suite"))
                             (:file "package")
                             (:file "suite" :depends-on ("package"))
                             #+nil(:file "syntax" :depends-on ("suite"))
                             (:file "type" :depends-on ("suite"))))))
