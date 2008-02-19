;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms)

(enable-sharp-boolean-syntax)

;;; These definitions need to be available by the time we are reading the other files, therefore
;;; they are in a standalone file.

(def macro debug-only (&body body)
  (if cl-rdbms-system:*load-as-production-p*
      (values)
      `(progn
         ,@body)))

(def macro debug-only* (&body body)
  `(unless cl-rdbms-system:*load-as-production-p*
     ,@body))

(def macro production-only (&body body)
  (if cl-rdbms-system:*load-as-production-p*
      `(progn
         ,@body)
      (values)))

(def macro production-only* (&body body)
  `(if cl-rdbms-system:*load-as-production-p*
       (progn
         ,@body)
       (values)))

(defun inline-declaration ()
  (if *load-as-production-p*
      '(inline begin commit rollback execute assert-transaction-in-progress in-transaction-p)
      (values)))

(defun file-header ()
  `(eval-always
    (declaim ,(inline-declaration))
    (setup-readtable)))

(defun setup-readtable ()
  (enable-sharp-boolean-syntax)
  (enable-sharp-l-syntax)
  (enable-sql-syntax))

#+#.(cl:when (cl:find-package "SWANK") '(:and))
(setup-swank-readtable-alist
 '("CL-RDBMS" "CL-RDBMS-TEST" "CL-RDBMS.POSTGRESQL" "CL-RDBMS.ORACLE") 'setup-readtable)
