;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms)

(defvar *log-level* (if *load-as-production-p*
                        +info+
                        +debug+))

(defvar *compile-time-log-level* (if *load-as-production-p*
                                     +debug+
                                     +dribble+))

(deflogger log ()
  :level *log-level*
  :compile-time-level *compile-time-log-level*
  :appenders ((debug-only*
                (make-instance 'brief-stream-log-appender :stream *debug-io*))))

(defclass sql-log-appender (stream-log-appender)
  ())

(deflogger sql-log ()
  :level +warn+
  :compile-time-level *compile-time-log-level*
  :appenders ((make-instance 'sql-log-appender :stream *debug-io*)))

(defun start-sql-recording ()
  (setf (log.level (get-logger 'sql-log)) +info+)
  (values))

(defun stop-sql-recording ()
  (setf (log.level (get-logger 'sql-log)) +warn+)
  (values))

(defun enable-sql-recording ()
  (start-sql-recording))

(defun disable-sql-recording ()
  (stop-sql-recording))

(defmethod append-message ((category log-category) (appender sql-log-appender) message level)
  (format (arnesi::log-stream appender) "~&~A~%" message))
