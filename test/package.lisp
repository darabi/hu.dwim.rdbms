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
                          #:log))

(in-package :hu.dwim.rdbms.test)

(import (let ((*package* (find-package :hu.dwim.rdbms)))
          ;; we intentionally not import all internal hu.dwim.rdbms symbols here to test the proper exporting of the symbols, too
          (read-from-string "(concatenate-symbol concatenate-string *sql-stream* eval-always rebind
                              connection-specification-of *database* *transaction*
                              with-transaction* process-sql-syntax-list compile-sexp-sql-column compile-sexp-sql-columns
                              compile-sexp-sql compile-sexp-sql-type expand-sql-ast-into-lambda-form
                              value-of compile-sexp-sql-binding-variable compile-sexp-sql-literal
                              log log.dribble log.debug log.info log.warn log.error)")))

(import-sql-syntax-node-names)

(import-sql-constructor-names)
