;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-user)

(defpackage #:cl-rdbms-system
  (:use :cl :asdf)
  (:export
   #:optimize-declaration
   #:*load-with-debug-p*))

(in-package #:cl-rdbms-system)

(defparameter *load-with-debug-p* t)

(defun optimize-declaration ()
  (if *load-with-debug-p*
      '(optimize (debug 3) (safety 3))
      '(optimize (speed 3) (debug 0) (safety 0))))

(defclass local-cl-source-file (cl-source-file)
  ())

(defmethod perform :around ((op operation) (component local-cl-source-file))
  (let ((*features* *features*))
    (when *load-with-debug-p*
      (pushnew :debug *features*))
    (call-next-method)))

(defsystem :cl-rdbms
  :version "0.1"
  :author ("Attila Lendvai <attila.lendvai@gmail.com>"
	   "Tamás Borbély <tomi.borbely@gmail.com>"
	   "Levente Mészáros <levente.meszaros@gmail.com>")
  :maintainer ("Attila Lendvai <attila.lendvai@gmail.com>"
               "Tamás Borbély <tomi.borbely@gmail.com>"
	       "Levente Mészáros <levente.meszaros@gmail.com>")
  :licence "BSD"
  :description "rdbms lib with sql syntax and sql backend abstractions"
  :depends-on (:arnesi :defclass-star :pg)
  :default-component-class local-cl-source-file
  :components
  ((:file "package")
   (:file "duplicates" :depends-on ("package"))
   (:file "configuration" :depends-on ("duplicates"))
   (:file "database" :depends-on ("configuration"))
   (:file "transaction" :depends-on ("database"))
   (:file "ddl" :depends-on ("configuration"))
   (:file "dml" :depends-on ("configuration"))
   (:module "syntax"
            :depends-on ("database" "transaction" "ddl")
            :components ((:file "format")
                         (:file "syntax" :depends-on ("format"))
                         (:file "sexp" :depends-on ("syntax"))
                         (:file "type" :depends-on ("syntax"))
                         (:file "constraint" :depends-on ("syntax"))
                         (:file "create-table" :depends-on ("syntax"))
                         (:file "drop-table" :depends-on ("syntax"))
                         (:file "alter-table" :depends-on ("create-table"))
                         (:file "expression" :depends-on ("syntax"))
                         (:file "insert" :depends-on ("syntax"))
                         (:file "select" :depends-on ("syntax" "expression"))
                         (:file "update" :depends-on ("syntax"))
                         (:file "delete" :depends-on ("syntax"))
                         (:file "sequence" :depends-on ("syntax"))))
   (:module "postgresql"
            :depends-on ("database" "transaction" "syntax")
            :components ((:file "database")
                         (:file "pg" :depends-on ("database"))
                         (:file "syntax" :depends-on ("database"))
                         (:file "type" :depends-on ("pg"))
                         (:file "ddl" :depends-on ("database"))))))

(defsystem :cl-rdbms-test
  :description "Tests for the cl-rdbms system."
  :depends-on (:cl-rdbms :fiveam)
  :components
  ((:file "test")))

(defmethod perform :after ((op load-op) (system (eql (find-system :cl-rdbms-test))))
  ;; globally enable the syntax in the repl thread
  (eval (read-from-string "(cl-rdbms::enable-sharp-boolean-syntax)")))

(defmethod perform ((op test-op) (system (eql (find-system :cl-rdbms))))
  (operate 'load-op :cl-rdbms)
  (in-package :cl-rdbms-test)
  (operate 'load-op :fiveam)
  (use-package :5am)
  (push :debug *features*)
  (operate 'load-op :cl-rdbms-test)
  (eval (read-from-string "(progn
                             (cl-rdbms::enable-sharp-boolean-syntax)
                             (5am:run!))"))
  (eval (read-from-string "(setf *database* *test-database*)"))
  (values))

(defmethod operation-done-p ((op test-op) (system (eql (find-system :cl-rdbms))))
  nil)
