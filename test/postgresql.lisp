;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.rdbms.test)

(def special-variable *postgresql-database* (make-instance 'hu.dwim.rdbms.postgresql:postgresql
                                                           :connection-specification '(:database "rdbms-test"
                                                                                       :user-name "rdbms-test"
                                                                                       :password "test123")))

(def test (test/postgresql :in test) ()
  (with-database *postgresql-database*
    (test/type)))
