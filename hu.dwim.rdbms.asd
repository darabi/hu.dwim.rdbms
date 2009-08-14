;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :asdf)

(load-system :hu.dwim.asdf)

(defsystem :hu.dwim.rdbms
  :class hu.dwim.system
  :author ("Attila Lendvai <attila.lendvai@gmail.com>"
           "Tamás Borbély <tomi.borbely@gmail.com>"
           "Levente Mészáros <levente.meszaros@gmail.com>")
  :licence "BSD / Public domain"
  :description "Relational database independent SQL abstractions"
  :depends-on (:babel
               :hu.dwim.def+hu.dwim.logger
               :hu.dwim.defclass-star+hu.dwim.def
               :hu.dwim.syntax-sugar+hu.dwim.walker
               :hu.dwim.syntax-sugar+swank
               :hu.dwim.walker
               :ironclad
               :local-time)
  :components ((:module "source"
                :components ((:file "package")
                             (:file "duplicates" :depends-on ("package"))
                             (:file "reader-macro" :depends-on ("duplicates") :pathname "syntax/reader-macro.lisp")
                             (:file "configuration" :depends-on ("duplicates" "reader-macro"))
                             (:file "logger" :depends-on ("configuration"))
                             (:module "generic"
                              :depends-on ("logger")
                              :components ((:file "database")
                                           (:file "transaction" :depends-on ("database"))
                                           (:file "cursor" :depends-on ("transaction"))
                                           (:file "ddl" :depends-on ("transaction"))
                                           (:file "dml" :depends-on ("transaction"))))
                             (:module "syntax"
                              :depends-on ("generic")
                              :components ((:file "format")
                                           (:file "syntax" :depends-on ("format"))
                                           (:file "sexp" :depends-on ("syntax" "expression"))
                                           (:file "type" :depends-on ("syntax"))
                                           (:file "constraint" :depends-on ("syntax"))
                                           (:file "create-table" :depends-on ("syntax" "expression"))
                                           (:file "drop-table" :depends-on ("syntax"))
                                           (:file "alter-table" :depends-on ("create-table"))
                                           (:file "expression" :depends-on ("syntax"))
                                           (:file "insert" :depends-on ("syntax"))
                                           (:file "select" :depends-on ("syntax" "expression"))
                                           (:file "update" :depends-on ("syntax"))
                                           (:file "delete" :depends-on ("syntax"))
                                           (:file "sequence" :depends-on ("syntax"))
                                           (:file "index" :depends-on ("syntax"))
                                           (:file "lock" :depends-on ("syntax"))))))))

(defmethod perform ((op test-op) (system (eql (find-system :hu.dwim.rdbms))))
  (test-system :hu.dwim.rdbms.oracle)
  (test-system :hu.dwim.rdbms.postgresql)
  (test-system :hu.dwim.rdbms.sqlite))
