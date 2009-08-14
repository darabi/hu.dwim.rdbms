;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :asdf)

(find-system :hu.dwim.rdbms)

(defsystem :hu.dwim.rdbms.sqlite
  :class hu.dwim.rdbms-backend-system
  :runtime-database-factory-form
  "(make-instance 'sqlite
                  :connection-specification
                  '(:file-name \"/tmp/perec-test\"))"
  :author ("Attila Lendvai <attila.lendvai@gmail.com>"
           "Tamás Borbély <tomi.borbely@gmail.com>"
           "Levente Mészáros <levente.meszaros@gmail.com>")
  :licence "BSD / Public domain"
  :description "Sqlite backend for hu.dwim.rdbms"
  :depends-on (:hu.dwim.common-lisp
               :hu.dwim.defclass-star
               :hu.dwim.rdbms
               :verrazano-runtime)
  :components ((:module "source"
                :components ((:module "sqlite"
                              :components ((:file "package")
                                           (:file "sqlite3-cffi-bindings" :depends-on ("package"))
                                           (:file "database" :depends-on ("sqlite3-cffi-bindings"))
                                           (:file "syntax" :depends-on ("database"))
                                           (:file "ddl" :depends-on ("database"))
                                           (:file "type" :depends-on ("database"))))))))
