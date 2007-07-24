;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms.postgresql)

#.(file-header)

(defclass* postgresql-transaction ()
  ())

(defparameter *unique-counter* 0)

(defun generate-unique-postgresql-name (base)
  (strcat base (incf *unique-counter*)))

(defconstant +maximum-rdbms-name-length+ 63)

;; this name mapping is not injective, different lisp names _may_ be mapped to the same rdbms name
(defmethod calculate-rdbms-name ((db postgresql) thing name)
  "Cuts off the end of names that are too long and appends the SXHASH of the original name."
  (calculate-rdbms-name-with-utf-8-length-limit name +maximum-rdbms-name-length+ :prefix "_"))
