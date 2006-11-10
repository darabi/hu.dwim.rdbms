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
  :depends-on (:arnesi :defclass-star)
  :default-component-class local-cl-source-file
  :components
  ((:file "package")
   (:file "duplicates" :depends-on ("package"))
   (:file "configuration" :depends-on ("duplicates"))
   (:file "database" :depends-on ("configuration"))
   (:file "transaction" :depends-on ("configuration"))
   (:module "sql-syntax"
            :depends-on ("configuration")
            :components ((:file "sql-syntax")
                         (:file "alter-table" :depends-on ("sql-syntax"))))
   (:module "backends"
            :depends-on ("database" "transaction")
            :components ((:file "postgresql")
                         (:file "postgresql-pg")))))

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
  (operate 'load-op :cl-rdbms-test)
  (funcall (read-from-string "5am:run!"))
  (values))

(defmethod operation-done-p ((op test-op) (system (eql (find-system :cl-rdbms))))
  nil)
