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
   #:project-relative-pathname
   #:*load-as-production-p*))

(in-package #:cl-rdbms-system)

(defun project-relative-pathname (path)
  (merge-pathnames path (component-pathname (find-system :cl-rdbms))))

(defparameter *load-as-production-p* t)

(defun optimize-declaration ()
  (if *load-as-production-p*
      '(optimize (speed 3) (debug 0) (safety 0))
      '(optimize (debug 3) (safety 3))))

(defclass local-cl-source-file (cl-source-file)
  ())

(defmethod perform :around ((op operation) (component local-cl-source-file))
  (let ((*features* *features*))
    (unless *load-as-production-p*
      (pushnew :debug *features*))
    (call-next-method)))

(defclass cl-rdbms-backend-system (system)
  ((database-factory-form :initarg :database-factory-form :accessor database-factory-form-of)))

(defsystem :cl-rdbms
  :version "1.0"
  :author ("Attila Lendvai <attila.lendvai@gmail.com>"
	   "Tamás Borbély <tomi.borbely@gmail.com>"
	   "Levente Mészáros <levente.meszaros@gmail.com>")
  :maintainer ("Attila Lendvai <attila.lendvai@gmail.com>"
               "Tamás Borbély <tomi.borbely@gmail.com>"
	       "Levente Mészáros <levente.meszaros@gmail.com>")
  :licence "BSD"
  :description "rdbms lib with sql syntax and sql backend abstractions"
  ;; TODO drop arnesi dependency
  :depends-on (:alexandria :iterate :cl-def :metabang-bind :defclass-star :arnesi :ironclad :local-time)
  :default-component-class local-cl-source-file
  :components
  ((:file "package")
   (:file "duplicates" :depends-on ("package"))
   (:file "reader-macro" :depends-on ("duplicates") :pathname "syntax/reader-macro.lisp")
   (:file "configuration" :depends-on ("duplicates" "reader-macro"))
   (:file "logging" :depends-on ("configuration"))
   (:file "database" :depends-on ("logging"))
   (:file "transaction" :depends-on ("database"))
   (:file "cursor" :depends-on ("transaction"))
   (:file "ddl" :depends-on ("transaction"))
   (:file "dml" :depends-on ("transaction"))
   (:module "syntax"
            :depends-on ("database" "transaction" "ddl" "logging")
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
                         (:file "sequence" :depends-on ("syntax"))
                         (:file "index" :depends-on ("syntax"))))))

(defsystem :cl-rdbms.postgresql
  :description "Common stuff for Postgresql backends for cl-rdbms"
  :depends-on (:arnesi :iterate :defclass-star :cl-rdbms)
  :default-component-class local-cl-source-file
  :components
  ((:module "postgresql"
            :serial t
            :components ((:file "package")
                         (:file "database")
                         (:file "syntax")
                         (:file "type")
                         (:file "ddl")))))

(defsystem :cl-rdbms.postmodern
  :class cl-rdbms-backend-system
  :database-factory-form "(make-instance 'postgresql-postmodern :connection-specification
                                         '(:database \"rdbms-test\" :user-name \"rdbms-test\" :password \"test123\"))"
  :description "cl-rdbms with Postmodern backend"
  :depends-on (:arnesi :iterate :defclass-star :cl-rdbms.postgresql :cl-postgres)
  :default-component-class local-cl-source-file
  :components
  ((:module "postgresql"
            :components ((:file "postmodern-backend")))))

(defsystem :cl-rdbms.oracle
  :class cl-rdbms-backend-system
  :database-factory-form "(make-instance 'oracle
                                         :connection-specification
                                         '(:datasource \"(ADDRESS =
                                                           (PROTOCOL = TCP)
                                                           (HOST = localhost)
                                                           (PORT = 1521))\"
                                           :user-name \"perec-test\"
                                           :password \"test123\"))"
  :description "cl-rdbms with Oracle backend"
  :depends-on (:arnesi :iterate :defclass-star :verrazano-runtime :cl-rdbms)
  :default-component-class local-cl-source-file
  :components
  ((:module "oracle"
            :serial t
            :components ((:file "package")
                         (:file "oracle-cffi-bindings")
                         (:file "database")
                         (:file "syntax")
                         (:file "ddl")
                         (:file "ffi-helpers")
                         (:file "conversion")
                         (:file "type")
                         (:file "backend")))))

(defsystem :cl-rdbms.sqlite
  :class cl-rdbms-backend-system
  :database-factory-form "(make-instance 'sqlite
                                         :connection-specification
                                         '(:file-name \"/tmp/perec-test\"))"
  :description "cl-rdbms with Sqlite backend"
  :depends-on (:arnesi :iterate :defclass-star :verrazano-runtime :cl-rdbms)
  :default-component-class local-cl-source-file
  :components
  ((:module "sqlite"
            :serial t
            :components ((:file "package")
                         (:file "sqlite3-cffi-bindings")
                         (:file "database")
                         (:file "syntax")
                         (:file "ddl")
                         (:file "type")))))

(defsystem :cl-rdbms-test
  :description "Tests for the cl-rdbms system."
  :depends-on (:iterate :stefil :cl-rdbms)
  :default-component-class local-cl-source-file
  :components
  ((:module "tests"
            :serial t
            :components ((:file "test-environment")
                         (:file "basic")
                         (:file "syntax")
                         (:file "types")))))

(defmethod perform :after ((op load-op) (system (eql (find-system :cl-rdbms-test))))
  (in-package :cl-rdbms-test)
  (eval (read-from-string "(cl-rdbms::enable-sharp-boolean-syntax)")))

(defmethod perform ((op test-op) (system (eql (find-system :cl-rdbms))))
  ;; we will test the postmodern backend by default
  (operate 'test-op :cl-rdbms.postmodern)
  (values))

(defmethod perform ((op test-op) (system cl-rdbms-backend-system))
  (operate 'load-op system)
  (in-package :cl-rdbms)
  ;; set it before compiling, so the SEXP SQL compiler will use the specified database type to format sql
  (eval (read-from-string
         (concatenate 'string "(progn
                                 (setf *database* " (database-factory-form-of system) "))")))
  (operate 'load-op :cl-rdbms-test)
  (in-package :cl-rdbms-test)
  (eval (read-from-string
         (concatenate 'string "(progn
                                 (setf *test-database* *database*))")))
  (declaim (optimize (debug 3)))
  (warn "(declaim (optimize (debug 3))) was issued to help later C-c C-c'ing")
  (eval (read-from-string "(progn
                             (stefil:funcall-test-with-feedback-message 'test))"))
  (warn "*database* was set to ~A to help REPL'ing" (eval (read-from-string "*database*")))
  (values))

(defmethod operation-done-p ((op test-op) (system (eql (find-system :cl-rdbms))))
  nil)

(defmethod operation-done-p ((op test-op) (system cl-rdbms-backend-system))
  nil)
