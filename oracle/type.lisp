;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms.oracle)

#.(file-header)

(defun sql-type-for-internal-type (description)
  (let ((type-name (first* description)))
    (macrolet ((sql-type-case (&body entries)
                 `(progn
                   ,@(loop for (name . body) :in entries
                           collect `(when (equalp type-name ,name)
                                     (return-from sql-type-for-internal-type (progn ,@body)))))))
      (sql-type-case ("bool" (make-instance 'sql-boolean-type)))
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
                    "number")
                   ((<= bit-size 16)
                    "number(5)")
                   ((<= bit-size 32)
                    "number(10)")
                   ((<= bit-size 64)
                    "number(19)")
                   (t
                    "number"))))

  (:method ((type sql-character-type))
           "varchar")

  (:method ((type sql-character-varying-type))
           "varchar")

  (:method ((type sql-timestamp-type))
           (if (with-timezone-p type)
               "timestamptz"
               "timestamp")))
