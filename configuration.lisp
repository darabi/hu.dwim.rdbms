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
      '(inline begin commit rollback execute-command)))

(defun file-header ()
  `(progn
    (declaim ,(inline-declaration))
    (enable-sharp-boolean-syntax)))



