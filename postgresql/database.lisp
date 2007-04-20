;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms.postgresql)

#.(file-header)

(defclass* postgresql (database)
  ())

(defclass* postgresql-transaction ()
  ())

(defparameter *unique-counter* 0)

(defun generate-unique-postgresql-name (base)
  (strcat base (incf *unique-counter*)))

