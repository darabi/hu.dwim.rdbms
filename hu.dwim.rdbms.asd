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
  :description "rdbms lib with sql syntax and sql backend abstractions"
  :depends-on (:babel
               :hu.dwim.common-lisp
               :hu.dwim.def
               :hu.dwim.defclass-star+hu.dwim.def
               :hu.dwim.logger
               :hu.dwim.syntax-sugar+hu.dwim.walker
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
  ;; we will test the postmodern backend by default
  (test-system :hu.dwim.rdbms.postmodern)
  (values))

(defclass hu.dwim.rdbms-backend-system (system-with-readtable)
  ((runtime-database-factory-form
    :initarg :runtime-database-factory-form
    :accessor runtime-database-factory-form-of)
   (compile-time-database-factory-form
    :initform nil
    :initarg :compile-time-database-factory-form
    :accessor compile-time-database-factory-form-of)))

(defmethod compile-time-database-factory-form-of :around ((self hu.dwim.rdbms-backend-system))
  (or (call-next-method)
      (runtime-database-factory-form-of self)))

(defmethod perform ((op test-op) (system hu.dwim.rdbms-backend-system))
  (load-system system)
  (in-package :hu.dwim.rdbms)
  ;; set it before compiling, so the SEXP SQL compiler will use the specified database type to format sql
  (progv
      (list (read-from-string "*database*"))
      (list (eval
             (read-from-string
              (compile-time-database-factory-form-of system))))
    (load-system :hu.dwim.rdbms.test))
  (in-package :hu.dwim.rdbms.test)
  (setf (symbol-value (read-from-string "*database*"))
        (eval
         (read-from-string
          (runtime-database-factory-form-of system))))
  (eval (read-from-string
         "(setf *test-database* *database*)"))
  (declaim (optimize (debug 3)))
  (warn "(declaim (optimize (debug 3))) was issued to help later C-c C-c'ing")
  (eval (read-from-string "(hu.dwim.stefil:funcall-test-with-feedback-message 'test)"))
  (warn "*database* was set to ~A to help REPL'ing" (eval (read-from-string "*database*")))
  (values))
