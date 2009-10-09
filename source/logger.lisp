;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.rdbms)

(def logger rdbms () :appenders ((debug-only* (make-instance 'brief-stream-appender :stream *debug-io*))))

(def class sql-appender (stream-appender)
  ())

(def logger sql (rdbms) :appenders ((make-instance 'sql-appender :stream *debug-io*)))

(def (function e) start-sql-recording ()
  (setf (log-level (find-logger 'sql)) +info+)
  (values))

(def (function e) stop-sql-recording ()
  (setf (log-level (find-logger 'sql)) +warn+)
  (values))

(def (function e) enable-sql-recording ()
  (start-sql-recording))

(def (function e) disable-sql-recording ()
  (stop-sql-recording))

(def method append-message ((category logger) (appender sql-appender) message level)
  (format (hu.dwim.logger::log-stream appender) "~&~A~%" message))
