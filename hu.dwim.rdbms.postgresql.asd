;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :asdf)

(find-system :hu.dwim.rdbms)

(defsystem :hu.dwim.rdbms.postgresql
  :class hu.dwim.system
  :setup-readtable-function-name "hu.dwim.rdbms::setup-readtable"
  :author ("Attila Lendvai <attila.lendvai@gmail.com>"
           "Tamás Borbély <tomi.borbely@gmail.com>"
           "Levente Mészáros <levente.meszaros@gmail.com>")
  :licence "BSD / Public domain"
  :description "Common stuff for Postgresql backends for hu.dwim.rdbms"
  :depends-on (:hu.dwim.common-lisp
               :hu.dwim.defclass-star
               :hu.dwim.rdbms)
  :components ((:module "source"
                :components ((:module "postgresql"
                              :components ((:file "package")
                                           (:file "database" :depends-on ("package"))
                                           (:file "syntax" :depends-on ("database"))
                                           (:file "type" :depends-on ("database"))
                                           (:file "ddl" :depends-on ("database"))))))))
