;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.rdbms.test)

(def special-variable *oracle-database* (make-instance 'hu.dwim.rdbms.oracle:oracle
                                                       :connection-specification '(:datasource "(DESCRIPTION=
                                                                                                  (ADDRESS =
                                                                                                    (PROTOCOL = TCP)
                                                                                                    (HOST = LOCALHOST)
                                                                                                    (PORT = 1521))
                                                                                                  (CONNECT_DATA =
                                                                                                    (SERVICE_NAME = xe.oracle.docker)))"
                                                                                   :user-name "hu.dwim.rdbms.test"
                                                                                   :password "engedjbe")))

(def test (test/oracle :in test) ()
  (with-database *oracle-database*
    (test/backend)))
