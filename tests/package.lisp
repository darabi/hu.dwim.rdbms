;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms)

(defpackage :cl-rdbms-test
  (:nicknames :rdbmst)

  (:use
   :common-lisp
   :iterate
   :stefil
   :alexandria
   :anaphora
   :cl-syntax-sugar
   :cl-rdbms
   :cl-yalog
   :cl-def
   :metabang-bind
   )

  (:shadowing-import-from :cl-rdbms
   #:log))

(in-package :cl-rdbms-test)

(import (let ((*package* (find-package :cl-rdbms)))
          ;; we intentionally not import all internal cl-rdbms symbols here to test the proper exporting of the symbols, too
          (read-from-string "(concatenate-symbol concatenate-string *sql-stream* eval-always rebind
                              connection-specification-of *database* *transaction*
                              with-transaction* process-sql-syntax-list compile-sexp-sql-column compile-sexp-sql-columns
                              compile-sexp-sql compile-sexp-sql-type expand-sql-ast-into-lambda-form
                              value-of compile-sexp-sql-binding-variable compile-sexp-sql-literal first* second* third*
                              log log.dribble log.debug log.info log.warn log.error)")))
(import-sql-syntax-node-names)
(import-sql-constructor-names)

