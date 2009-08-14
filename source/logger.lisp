;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.rdbms)

(def special-variable *log-level* (if *load-as-production?*
                                      +info+
                                      +debug+))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (def special-variable *compile-time-log-level* (if *load-as-production?*
                                                     +debug+
                                                     +dribble+)))

(def logger log ()
  :level *log-level*
  :compile-time-level *compile-time-log-level*
  :appenders ((debug-only*
                (make-instance 'brief-stream-log-appender :stream *debug-io*))))

(def class sql-log-appender (stream-log-appender)
  ())

(def logger sql-log ()
  :level +warn+
  :compile-time-level *compile-time-log-level*
  :appenders ((make-instance 'sql-log-appender :stream *debug-io*)))

(def (function e) start-sql-recording ()
  (setf (log-level (find-logger 'sql-log)) +info+)
  (values))

(def (function e) stop-sql-recording ()
  (setf (log-level (find-logger 'sql-log)) +warn+)
  (values))

(def (function e) enable-sql-recording ()
  (start-sql-recording))

(def (function e) disable-sql-recording ()
  (stop-sql-recording))

(def method append-message ((category log-category) (appender sql-log-appender) message level)
  (format (hu.dwim.logger::log-stream appender) "~&~A~%" message))
