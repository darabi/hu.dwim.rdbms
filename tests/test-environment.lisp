;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms-test)

#.(cl-rdbms::file-header)

;;; please note that cl-perec tests cl-rdbms throughout, this is only a limited test file here

(eval-always
  (use-package :stefil)
  (import (let ((*package* (find-package :cl-rdbms)))
            ;; we intentionally not import all internal cl-rdbms symbols here to test the proper exporting of the symbols, too
            (read-from-string "(enable-sharp-boolean-syntax
                                connection-specification-of *database* *transaction*
                                with-transaction* process-sql-syntax-list compile-sexp-sql-column compile-sexp-sql-columns
                                compile-sexp-sql compile-sexp-sql-type
                                value-of compile-sexp-sql-binding-variable compile-sexp-sql-literal first* second* third*
                                log log.dribble log.debug log.info log.warn log.error)")))
  (import-sql-syntax-node-names))

(in-root-suite)

(def suite test)

(in-suite test)

;; e.g. (make-instance 'postgresql-postmodern :connection-specification '(:database "dwim" :user-name "root" :password "admin123"))
(defvar *test-database*)

(defmacro with-test-transaction (&body body)
  `(with-transaction* (:database *test-database*)
    ,@body))

(def definer test* (name args &body body)
  `(def test ,name ,args
     (with-database *test-database*
       ,@body)))

