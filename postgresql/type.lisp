;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms.postgresql)

#.(file-header)

(defmethod format-sql-syntax-node ((type sql-float-type) (database postgresql))
  (let ((bit-size (bit-size-of type)))
    (cond ((<= bit-size 32)
           (format-string "FLOAT4"))
          ((<= bit-size 64)
           (format-string "FLOAT8")))))

(defmethod format-sql-syntax-node ((type sql-character-large-object-type) (database postgresql))
  (format-string "TEXT"))

(defmethod format-sql-syntax-node ((type sql-binary-large-object-type) (database postgresql))
  (format-string "BYTEA"))

(defun sql-type-for-internal-type (description)
  (let ((type-name (first description)))
    (macrolet ((sql-type-case (&body entries)
                 `(progn
                   ,@(loop for (name . body) :in entries
                           collect `(when (equalp type-name ,name)
                                     (return-from sql-type-for-internal-type (progn ,@body)))))))
      (sql-type-case ("int2" (make-instance 'sql-integer-type :bit-size 16))
                     ("int4" (make-instance 'sql-integer-type :bit-size 32))
                     ("int8" (make-instance 'sql-integer-type :bit-size 64))
                     ("float4" (make-instance 'sql-float-type :bit-size 32))
                     ("float8" (make-instance 'sql-float-type :bit-size 64))
                     ("numeric" (make-instance 'sql-numeric-type))
                     ("bool" (make-instance 'sql-boolean-type))
                     ("char" (make-instance 'sql-character-type :size (- (third description) 4)))
                     ("bpchar" (make-instance 'sql-character-type :size (- (third description) 4)))
                     ("varchar" (make-instance 'sql-character-varying-type :size (- (third description) 4)))
                     ("text" (make-instance 'sql-character-large-object-type))
                     ("bytea" (make-instance 'sql-binary-large-object-type))
                     ("date" (make-instance 'sql-date-type))
                     ("time" (make-instance 'sql-time-type))
                     ("timestamp" (make-instance 'sql-timestamp-type :with-timezone #f))
                     ("timestamptz" (make-instance 'sql-timestamp-type :with-timezone #t)))
      (error "Unknown internal type"))))

(defgeneric internal-type-for-sql-type (type)
  (:method (type)
           (let ((str (format-sql-to-string type)))
             (string-downcase
              (aif (position #\( str :test #'char=)
                   (subseq str 0 it)
                   str))))

  (:method ((type sql-integer-type))
           (let ((bit-size (bit-size-of type)))
             (cond ((null bit-size)
                    "numeric")
                   ((<= bit-size 16)
                    "int2")
                   ((<= bit-size 32)
                    "int4")
                   ((<= bit-size 64)
                    "int8")
                   (t
                    "numeric"))))

  (:method ((type sql-character-type))
           "varchar")

  (:method ((type sql-character-varying-type))
           "varchar")

  (:method ((type sql-timestamp-type))
           (if (with-timezone-p type)
               "timestamptz"
               "timestamp")))

(defgeneric binding-type-for-sql-type (sql-type database))


