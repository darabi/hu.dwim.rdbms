;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :asdf)

(load-system :hu.dwim.asdf)

(defsystem :hu.dwim.rdbms.oracle.test
  :class hu.dwim.test-system
  :package-name :hu.dwim.rdbms.test
  :author ("Attila Lendvai <attila.lendvai@gmail.com>"
           "Levente Mészáros <levente.meszaros@gmail.com>"
           "Tamás Borbély <tomi.borbely@gmail.com>")
  :licence "BSD / Public domain"
  :description "Test suite for hu.dwim.rdbms.oracle"
  :depends-on (:hu.dwim.rdbms.oracle
               :hu.dwim.rdbms.test)
  :components ((:module "test"
                :components ((:file "oracle")))))
