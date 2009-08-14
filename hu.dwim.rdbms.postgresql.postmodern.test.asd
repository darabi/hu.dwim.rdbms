;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :asdf)

(load-system :hu.dwim.asdf)

(defsystem :hu.dwim.rdbms.postgresql.postmodern.test
  :class hu.dwim.test-system
  :package-name :hu.dwim.rdbms.test
  :author ("Attila Lendvai <attila.lendvai@gmail.com>"
           "Levente Mészáros <levente.meszaros@gmail.com>"
           "Tamás Borbély <tomi.borbely@gmail.com>")
  :licence "BSD / Public domain"
  :description "Test suite for hu.dwim.rdbms.postgresql.postmodern"
  :depends-on (:hu.dwim.rdbms.postgresql.postmodern
               :hu.dwim.rdbms.test)
  :components ((:module "test"
                :components ((:file "postmodern")))))

(defmethod call-in-system-environment ((operation load-op) (system (eql (find-system :hu.dwim.rdbms.postgresql.postmodern.test))) function)
  (progv
      (list (read-from-string "hu.dwim.rdbms:*database*"))
      (list (eval (read-from-string "(make-instance 'hu.dwim.rdbms.postgresql:postgresql-postmodern)")))
    (call-next-method)))
