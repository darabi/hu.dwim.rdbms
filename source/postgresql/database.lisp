;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.rdbms.postgresql)

(def (class* e) postgresql (database)
  ((muffle-warnings #f :type boolean :accessor muffle-warnings?)))

(def constructor postgresql ()
  ;; When a rational number is passed into a query (as per
  ;; to-sql-string), but it can not be expressed within 38 decimal
  ;; digits (for example 1/3), it will be truncated, and lose some
  ;; precision. Set this variable to nil to suppress that behaviour and
  ;; raise an error instead.
  ;;
  ;; from: https://marijnhaverbeke.nl/postmodern/cl-postgres.html
  (setf cl-postgres:*silently-truncate-rationals* nil)
  (set-rdbms-cl-postgres-readers))

(def special-variable *unique-counter* 0)

(def (function i) generate-unique-postgresql-name (base)
  (string+ (string base) (princ-to-string (incf *unique-counter*))))

(def constant +maximum-rdbms-name-length+ 63)

;; this name mapping is not injective, different lisp names _may_ be mapped to the same rdbms name
(def method calculate-rdbms-name ((db (eql :postgresql)) thing name)
  "Cuts off the end of names that are too long and appends the hash of the original name."
  (calculate-rdbms-name-with-utf-8-length-limit name +maximum-rdbms-name-length+ :prefix "_"))

(def method calculate-rdbms-name ((db postgresql) thing name)
  (calculate-rdbms-name :postgresql thing name))
