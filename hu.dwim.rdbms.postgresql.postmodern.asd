;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :asdf)

(load-system :hu.dwim.asdf)

(defsystem :hu.dwim.rdbms.postgresql.postmodern
  :class hu.dwim.system
  :package-name :hu.dwim.rdbms
  :author ("Attila Lendvai <attila.lendvai@gmail.com>"
           "Tamás Borbély <tomi.borbely@gmail.com>"
           "Levente Mészáros <levente.meszaros@gmail.com>")
  :licence "BSD / Public domain"
  :description "Postmodern backend for hu.dwim.rdbms"
  :depends-on (:cl-postgres
               :hu.dwim.rdbms.postgresql)
  :components ((:module "source"
                :components ((:module "postgresql"
                              :components ((:file "postmodern-backend")))))))

(defmethod perform :after ((op develop-op) (system (eql (find-system :hu.dwim.rdbms.postgresql.postmodern))))
  (let ((database-variable (read-from-string "hu.dwim.rdbms::*database*")))
    (unless (boundp database-variable)
      (setf (symbol-value database-variable)
            (symbol-value (read-from-string "hu.dwim.rdbms.test::*postgresql-database*"))))))
