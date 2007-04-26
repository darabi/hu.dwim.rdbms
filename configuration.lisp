;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms)

(enable-sharp-boolean-syntax)

;;; These definitions need to be available by the time we are reading the other files, therefore
;;; they are in a standalone file.

(defmacro debug-only (&body body)
  #+debug`(progn ,@body)
  #-debug(declare (ignore body)))

(defun inline-declaration ()
  (if *load-with-debug-p*
      (values)
      '(inline begin commit rollback execute assert-transaction-in-progress in-transaction-p)))

(defun file-header ()
  `(eval-always
    (declaim ,(inline-declaration))
    (setup-readtable)))

(defun setup-readtable ()
  (enable-sharp-boolean-syntax)
  (enable-sharp-l-syntax))

#+#.(cl:when (cl:find-package "SWANK") '(:and))
(unless (assoc "CL-RDBMS" swank:*readtable-alist* :test #'string=)
  (let ((*readtable* (copy-readtable)))
    (setup-readtable)
    (flet ((doit (&rest packages)
             (dolist (package packages)
               (push (cons package *readtable*) swank:*readtable-alist*))))
      (doit "CL-RDBMS" "CL-RDBMS.POSTGRESQL" "CL-RDBMS-TEST"))))
