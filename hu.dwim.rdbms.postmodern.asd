;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :asdf)

(find-system :hu.dwim.rdbms)

(defsystem :hu.dwim.rdbms.postmodern
  :class hu.dwim.rdbms-backend-system
  :setup-readtable-function-name "hu.dwim.rdbms::setup-readtable"
  :author ("Attila Lendvai <attila.lendvai@gmail.com>"
           "Tamás Borbély <tomi.borbely@gmail.com>"
           "Levente Mészáros <levente.meszaros@gmail.com>")
  :runtime-database-factory-form
  "(make-instance 'postgresql-postmodern :connection-specification
                  '(:database \"rdbms-test\" :user-name \"rdbms-test\" :password \"test123\"))"
  :compile-time-database-factory-form "(make-instance 'postgresql)"
  :author ("Attila Lendvai <attila.lendvai@gmail.com>"
           "Tamás Borbély <tomi.borbely@gmail.com>"
           "Levente Mészáros <levente.meszaros@gmail.com>")
  :licence "BSD / Public domain"
  :description "Postmodern backend for hu.dwim.rdbms"
  :depends-on (:cl-postgres
               :hu.dwim.common-lisp
               :hu.dwim.defclass-star
               :hu.dwim.rdbms.postgresql)
  :components ((:module "source"
                :components ((:module "postgresql"
                              :components ((:file "postmodern-backend")))))))
