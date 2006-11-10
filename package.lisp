;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-user)

(defpackage :cl-rdbms
  (:shadow #:log)
  
  (:use :cl :cl-rdbms-system :arnesi :defclass-star)

  (:export)

  ;; for debug purposes
  (:export))

(defpackage :cl-rdbms-test
  (:use :cl :cl-rdbms :arnesi))

(in-package :cl-rdbms)

(deflogger log ()
  :level (or #+debug +dribble+ +warn+)
  :compile-time-level (or #+debug +dribble+ +warn+)
  :appender (make-instance 'brief-stream-log-appender :stream *debug-io*))
