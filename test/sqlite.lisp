;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.rdbms.test)

(def special-variable *sqlite-database* (make-instance 'hu.dwim.rdbms:sqlite :connection-specification '(:file-name "/tmp/perec-test")))

(def test (test/sqlite :in test) ()
  (with-database *sqlite-database*
    (test/type)))
