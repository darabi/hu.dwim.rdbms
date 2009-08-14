;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :asdf)

(load-system :hu.dwim.asdf)

(defsystem :hu.dwim.rdbms.sqlite
  :class hu.dwim.system
  :package-name :hu.dwim.rdbms
  :author ("Attila Lendvai <attila.lendvai@gmail.com>"
           "Tamás Borbély <tomi.borbely@gmail.com>"
           "Levente Mészáros <levente.meszaros@gmail.com>")
  :licence "BSD / Public domain"
  :description "Sqlite backend for hu.dwim.rdbms"
  :depends-on (:hu.dwim.rdbms
               :verrazano-runtime)
  :components ((:module "source"
                :components ((:module "sqlite"
                              :components ((:file "package")
                                           (:file "cffi" :depends-on ("package"))
                                           (:file "database" :depends-on ("cffi"))
                                           (:file "syntax" :depends-on ("database"))
                                           (:file "ddl" :depends-on ("database"))
                                           (:file "type" :depends-on ("database"))))))))
