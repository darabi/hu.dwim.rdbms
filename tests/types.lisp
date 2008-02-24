;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms-test)

#.(cl-rdbms::file-header)

(enable-sql-syntax)

(def suite (types :in-suite 'test))

(in-suite types)

(def definer type-test (name type &body values)
  (bind ((sql-type (compile-sexp-sql-type type)))
    `(def test* ,name ()
       (unwind-protect
            (with-transaction
              ;; the first , is unquote relative to the sql syntax, the second is relative to `
              (execute-ddl [create table alma ((a ,,sql-type))])
              ,@(iter (for (comparator value expected) :in values)
                      (unless expected
                        (setf expected value))
                      (collect `(progn
                                  (execute [insert alma (a) (,(sql-literal :value ,value :type ,sql-type))])
                                  (is (funcall ',comparator
                                               (first* (first* (execute [select * alma] :result-type 'list)))
                                               ,expected))
                                  (execute [delete alma])))))
         (ignore-errors
           (execute-ddl [drop table alma]))))))

(def definer simple-type-test (name type &body values)
  `(def type-test ,name ,type
       ,@(mapcar
          (lambda (value)
            (list 'equalp value value))
          values)))

(def simple-type-test test/types/boolean boolean
  t
  nil)

(def simple-type-test test/types/char (char 10)
  "1234567890"
  "áéíóöőúüű ")

(def simple-type-test test/types/varchar (varchar 10)
  "1234567890"
  "áéíóöőúüű")

(def simple-type-test test/types/clob clob
  "1234567890"
  "áéíóöőúüű")

(def simple-type-test test/types/int8 (integer 8)
  0
  1
  -1
  127
  -128)

(def simple-type-test test/types/int16 (integer 16)
  0
  1
  -1
  32767
  -32768)

(def simple-type-test test/types/int32 (integer 32)
  0
  1
  -1
  2147483647
  -2147483648)

(def simple-type-test test/types/integer integer
  0
  1
  -1
  12345678901234567890123456789012345678
  -12345678901234567890123456789012345678)

(def simple-type-test test/types/blob blob
  (coerce #(1 2 3 4 5 6 7 8 9 0) '(vector (unsigned-byte 8))))

(def type-test test/types/date date
  ;; TODO (signals 'error (local-time:now))
  (local-time:local-time= (local-time:parse-datestring "1000-01-01"))
  (local-time:local-time= (local-time:parse-datestring "0000-01-01"))
  (local-time:local-time= (local-time:parse-datestring "2000-01-01"))
  (local-time:local-time= (local-time:parse-datestring "3000-01-01")))

(def type-test test/types/time time
  ;; TODO (signals 'error (local-time:parse-timestring "06:06:06+02:00"))
  (local-time:local-time= (local-time:parse-timestring "06:06:06Z"))
  (local-time:local-time= (local-time:parse-timestring "00:00:00Z"))
  (local-time:local-time= (local-time:parse-timestring "23:59:59Z")))

;; TODO take tz into account when comparing
(def type-test test/types/timestamp (timestamp #f)
  ;; TODO (signals 'error (local-time:parse-timestring "2006-06-06T06:06:06+02:00"))
  (local-time:local-time= (local-time:parse-timestring "2006-06-06T06:06:06Z")))

(def type-test test/types/timestamp-tz (timestamp #t)
  (local-time:local-time= (local-time:parse-timestring "2006-06-06T06:06:06Z"))
  (local-time:local-time= (local-time:parse-timestring "2006-06-06T06:06:06-01:00"))
  (local-time:local-time= (local-time:parse-timestring "2006-06-06T06:06:06-01:30"))
  (local-time:local-time= (local-time:parse-timestring "2006-06-06T06:06:06+01:00"))
  (local-time:local-time= (local-time:parse-timestring "2006-06-06T06:06:06+01:25")))
