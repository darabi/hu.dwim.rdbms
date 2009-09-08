;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.rdbms.documentation
  :class hu.dwim.documentation-system
  :author ("Attila Lendvai <attila.lendvai@gmail.com>"
           "Levente Mészáros <levente.meszaros@gmail.com>"
           "Tamás Borbély <tomi.borbely@gmail.com>")
  :licence "BSD / Public domain"
  :description "Documentation for hu.dwim.rdbms"
  :depends-on (:hu.dwim.rdbms.oracle.test
               :hu.dwim.rdbms.postgresql.test
               :hu.dwim.rdbms.sqlite.test
               :hu.dwim.wui)
  :components ((:module "documentation"
                :components ((:file "package")))))
