;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms)

#.(file-header)

(defmethod format-sql-syntax-node ((type sql-float-type) (database postgresql))
  (let ((bit-size (bit-size-of type)))
    (cond ((<= bit-size 32)
           (format-string "FLOAT4"))
          ((<= bit-size 64)
           (format-string "FLOAT8")))))

(defun sql-column-type-for-internal-type (description)
  (let ((type-name (first description)))
    (macrolet ((sql-type-case (&body entries)
                 `(progn
                   ,@(loop for (name . body) :in entries
                           collect `(when (equalp type-name ,name)
                                     (return-from sql-column-type-for-internal-type (progn ,@body)))))))
      (sql-type-case ("int2" (make-instance 'sql-integer-type :bit-size 16))
                     ("int4" (make-instance 'sql-integer-type :bit-size 32))
                     ("int8" (make-instance 'sql-integer-type :bit-size 64))
                     ("float4" (make-instance 'sql-float-type :bit-size 32))
                     ("float8" (make-instance 'sql-float-type :bit-size 64))
                     ("numeric" (make-instance 'sql-integer-type))
                     ("bool" (make-instance 'sql-boolean-type))
                     ("varchar" (make-instance 'sql-varchar-type :size (- (third description) 4)))
                     ("date" (make-instance 'sql-date-type))
                     ("time" (make-instance 'sql-time-type))
                     ("timestamp" (make-instance 'sql-timestamp-type)))
      (error "Unknown type"))))

