;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.rdbms.test)

(def suite* (test/type :in nil))

(def definer type-test (name type &body values)
  `(def test ,name ()
     (with-transaction
       (execute-ddl [drop table alma])
       ;; the first , is unquote relative to the sql syntax, the second is relative to `
       ;; TODO [create table alma ((a ,,sql-type))] should just work fine...
       (execute-ddl [create table alma ((a ,(compile-sexp-sql-type ',type)))])
       ,@(iter (for (comparator value expected) :in values)
               (unless expected
                 (setf expected value))
               (collect `(progn
                           (execute [insert alma (a) (,(sql-literal :value ,value :type (compile-sexp-sql-type ',type)))])
                           (is (funcall ',comparator
                                        (first-elt (first-elt (execute [select * alma] :result-type 'list)))
                                        ,expected))
                           (execute [delete alma])))))))

(def definer simple-type-test (name type &body values)
  `(def type-test ,name ,type
       ,@(mapcar
          (lambda (value)
            (setf value (ensure-list value))
            (list 'equalp (first value) (if (rest value)
                                            (second value)
                                            (first value))))
          values)))

(def simple-type-test test/type/boolean boolean
  :null
  t
  nil)

(def simple-type-test test/type/char (char 10)
  (nil :null)
  "1234567890"
  "áéíóöőúüű ")

(def simple-type-test test/type/varchar (varchar 10)
  (nil :null)
  "1234567890"
  "áéíóöőúüű")

(def simple-type-test test/type/clob clob
  (nil :null)
  "1234567890"
  "áéíóöőúüű")

(def simple-type-test test/type/int8 (integer 8)
  (nil :null)
  0
  1
  -1
  127
  -128)

(def simple-type-test test/type/int16 (integer 16)
  (nil :null)
  0
  1
  -1
  32767
  -32768)

(def simple-type-test test/type/int32 (integer 32)
  (nil :null)
  0
  1
  -1
  2147483647
  -2147483648)

(def simple-type-test test/type/integer integer
  (nil :null)
  0
  1
  -1
  12345678901234567890123456789012345678
  -12345678901234567890123456789012345678)

(def simple-type-test test/type/float float
  (nil :null)
  0.0
  1.0
  -1.0
  0.5
  -0.5
  1.23e+9
  -1.23e+9)

(def simple-type-test test/type/double-float (float 64)
  (nil :null)
  0.0
  1.0
  -1.0
  0.5
  -0.5
  1.23e+9
  -1.23e+9
  ;; we test here that ratio's precision is lost only up to double-float
  (1/3 #.(coerce 1/3 'double-float)))

(def simple-type-test test/type/blob blob
  (nil :null)
  #.(coerce #(1 2 3 4 5 6 7 8 9 0) '(vector (unsigned-byte 8))))

(def type-test test/type/date date
  (eq nil :null)
  ;; TODO (signals 'error (local-time:now))
  (local-time:timestamp= (local-time:parse-datestring "1000-01-01"))
  (local-time:timestamp= (local-time:parse-datestring "0001-01-01"))
  (local-time:timestamp= (local-time:parse-datestring "2000-01-01"))
  (local-time:timestamp= (local-time:parse-datestring "3000-01-01")))

(def type-test test/type/time time
  (eq nil :null)
  ;; TODO (signals 'error (local-time:parse-timestring "06:06:06+02:00"))
  (local-time:timestamp= (local-time:parse-timestring "06:06:06Z"))
  (local-time:timestamp= (local-time:parse-timestring "00:00:00Z"))
  (local-time:timestamp= (local-time:parse-timestring "23:59:59Z")))

(def type-test test/type/timestamp (timestamp #f)
  (eq nil :null)
  (local-time:timestamp= (local-time:parse-timestring "2006-06-06T06:06:06Z")))

;; TODO local-time:timestamp has no timezone information anymore... if we want to really test this here, then we need to introduce a type representing a tuple of (timestamp, timezone)
(def type-test test/type/timestamp-tz (timestamp #t)
  (eq nil :null)
  (local-time:timestamp= (local-time:parse-timestring "2006-06-06T06:06:06Z"))
  (local-time:timestamp= (local-time:parse-timestring "2006-06-06T06:06:06-01:00"))
  (local-time:timestamp= (local-time:parse-timestring "2006-06-06T06:06:06-01:30"))
  (local-time:timestamp= (local-time:parse-timestring "2006-06-06T06:06:06+01:00"))
  (local-time:timestamp= (local-time:parse-timestring "2006-06-06T06:06:06+01:25"))
  (local-time:timestamp= (local-time:parse-timestring "2006-01-01T01:01:01+01:25")))
