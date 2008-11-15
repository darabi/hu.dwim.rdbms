;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms-test)

;;; please note that cl-perec tests cl-rdbms throughout, this is only a limited test file here

(defsuite* (test :in root-suite))

;; e.g. (make-instance 'postgresql-postmodern :connection-specification '(:database "dwim" :user-name "root" :password "admin123"))
(defvar *test-database*)

(defmacro with-test-transaction (&body body)
  `(with-transaction* (:database *test-database*)
    ,@body))

(def definer test* (name args &body body)
  `(def test ,name ,args
     (with-database *test-database*
       ,@body)))

