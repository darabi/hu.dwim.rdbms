;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.rdbms.test)

(defun setup-readtable ()
  (enable-sharp-boolean-syntax)
  (enable-sharp-l-syntax)
  (enable-sql-syntax))

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

