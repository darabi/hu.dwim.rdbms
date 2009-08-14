;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :asdf)

(find-system :hu.dwim.rdbms)

(defsystem :hu.dwim.rdbms.oracle
  :class hu.dwim.rdbms-backend-system
  :runtime-database-factory-form
  "(make-instance 'oracle
                  :connection-specification
                  '(:datasource \"(ADDRESS =
                                    (PROTOCOL = TCP)
                                    (HOST = localhost)
                                    (PORT = 1521))\"
                    :user-name \"perec-test\"
                    :password \"test123\"))"
  :author ("Attila Lendvai <attila.lendvai@gmail.com>"
           "Tamás Borbély <tomi.borbely@gmail.com>"
           "Levente Mészáros <levente.meszaros@gmail.com>")
  :licence "BSD / Public domain"
  :description "Oracle backend for hu.dwim.rdbms"
  :depends-on (:hu.dwim.common-lisp
               :hu.dwim.defclass-star
               :hu.dwim.rdbms
               :verrazano-runtime)
  :components ((:module "source"
                :components ((:module "oracle"
                              :components ((:file "package")
                                           (:file "oracle-cffi-bindings" :depends-on ("package"))
                                           (:file "database" :depends-on ("oracle-cffi-bindings"))
                                           (:file "syntax" :depends-on ("database"))
                                           (:file "ddl" :depends-on ("database"))
                                           (:file "ffi-helpers" :depends-on ("database"))
                                           (:file "conversion" :depends-on ("database"))
                                           (:file "type" :depends-on ("database"))
                                           (:file "backend" :depends-on ("database"))))))))
