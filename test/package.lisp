;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :common-lisp-user)

(defpackage :hu.dwim.rdbms.test
  (:use :hu.dwim.common-lisp
        :hu.dwim.def
        :hu.dwim.logger
        :hu.dwim.rdbms
        :hu.dwim.stefil
        :hu.dwim.syntax-sugar)

  (:shadowing-import-from :hu.dwim.rdbms
                          #:log)

  ;; we intentionally not import all internal hu.dwim.rdbms symbols here to test the proper exporting of the symbols, too
  (:import-from :hu.dwim.rdbms
                #:*database*
                #:*sql-stream*
                #:*transaction*
                #:compile-sexp-sql
                #:compile-sexp-sql-binding-variable
                #:compile-sexp-sql-column
                #:compile-sexp-sql-columns
                #:compile-sexp-sql-literal
                #:compile-sexp-sql-type
                #:concatenate-string
                #:concatenate-symbol
                #:connection-specification-of
                #:eval-always
                #:expand-sql-ast-into-lambda-form
                #:log.debug
                #:log.dribble
                #:log.error
                #:log.info
                #:log.warn
                #:rebind
                #:value-of
                #:with-transaction*))

(in-package :hu.dwim.rdbms.test)

(import-sql-syntax-node-names)

(import-sql-constructor-names)
