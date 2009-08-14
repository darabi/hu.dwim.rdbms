;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.rdbms.test)

(def special-variable *oracle-database* (make-instance 'hu.dwim.rdbms:oracle
                                                       :connection-specification '(:datasource \"(ADDRESS =
                                                                                                  (PROTOCOL = TCP)
                                                                                                  (HOST = localhost)
                                                                                                  (PORT = 1521))\"
                                                                                   :user-name \"perec-test\"
                                                                                   :password \"test123\")))


(def teste (test/oracle :in test) ()
  (with-database *oracle-database*
    (test/type)))
