;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.rdbms)

;; Types from:
;; http://savage.net.au/SQL/sql-2003-2.bnf.html

(define-syntax-node sql-type (sql-syntax-node)
  ()
  (:documentation "Base class for all SQL types."))

(define-syntax-node sql-simple-type (sql-type)
  ()
  (:documentation "Types without parameters"))

(define-syntax-node sql-boolean-type (sql-simple-type)
  ()
  (:format-sql-syntax-node
   (format-string "BOOL")))

(define-syntax-node sql-numeric-type (sql-simple-type)
  ()
  (:format-sql-syntax-node
   (format-string "NUMERIC")))

(define-syntax-node sql-bit-sized-type (sql-type)
  ((bit-size
    nil
    :type (or null integer))))

(define-syntax-node sql-float-type (sql-bit-sized-type)
  ()
  (:format-sql-syntax-node
   (assert (and bit-size
                (<= 32 bit-size 64)))
   (cond ((<= bit-size 32)
          (format-string "REAL"))
         ((<= bit-size 64)
          (format-string "DOUBLE PRECISION")))))

(define-syntax-node sql-integer-type (sql-bit-sized-type)
  ()
  (:format-sql-syntax-node
   (cond ((null bit-size)
          (format-string "NUMERIC"))
         ((<= bit-size 16)
          (format-string "SMALLINT"))
         ((<= bit-size 32)
          (format-string "INT"))
         ((<= bit-size 64)
          (format-string "BIGINT"))
         (t
          (format-string "NUMERIC")))))

(define-syntax-node sql-sized-type (sql-type)
  ((size
    nil
    :type (or null (integer 0)))))

(define-syntax-node sql-string-type (sql-sized-type)
  ())

(define-syntax-node sql-character-type (sql-string-type)
  ()
  (:format-sql-syntax-node
   (format-string "CHARACTER")
   (format-size size)))

(define-syntax-node sql-character-varying-type (sql-string-type)
  ()
  (:format-sql-syntax-node
   (format-string "CHARACTER VARYING")
   (format-size size)))

(define-syntax-node sql-character-large-object-type (sql-string-type)
  ()
  (:format-sql-syntax-node
   (format-string "CHARACTER LARGE OBJECT")
   (format-size size)))

(define-syntax-node sql-date-type (sql-simple-type)
  ()
  (:format-sql-syntax-node
   (format-string "DATE")))

(define-syntax-node sql-time-type (sql-simple-type)
  ()
  (:format-sql-syntax-node
   (format-string "TIME")))

(define-syntax-node sql-timestamp-type (sql-simple-type)
  ((with-timezone
     #t
     :type boolean))
  (:format-sql-syntax-node
   (if with-timezone
       (format-string "TIMESTAMP WITH TIME ZONE")
       (format-string "TIMESTAMP"))))

(define-syntax-node sql-interval-type (sql-simple-type)
  ()
  (:format-sql-syntax-node
   (format-string "INTERVAL")))

(define-syntax-node sql-binary-large-object-type (sql-sized-type)
  ()
  (:format-sql-syntax-node
   (format-string "BINARY LARGE OBJECT")
   (format-size size)))

(defmethod equal-type-p ((type-1 sql-simple-type) (type-2 sql-simple-type) database)
  (eq (class-of type-1) (class-of type-2)))

(defmethod equal-type-p ((type-1 sql-bit-sized-type) (type-2 sql-bit-sized-type) database)
  (eq (bit-size-of type-1) (bit-size-of type-2)))

(defmethod equal-type-p ((type-1 sql-sized-type) (type-2 sql-sized-type) database)
  (and (eq (class-of type-1) (class-of type-2))
       (eq (size-of type-1) (size-of type-2))))

(defmethod rdbms-type-for ((type sql-bit-sized-type) database)
  (let ((bit-size (bit-size-of type)))
    (cond ((null bit-size)
           (make-instance (class-of type)))
          ((<= bit-size 16)
           (make-instance (class-of type) :bit-size 16))
          ((<= bit-size 32)
           (make-instance (class-of type) :bit-size 32))
          ((<= bit-size 64)
           (make-instance (class-of type) :bit-size 64))
          (t
           (make-instance (class-of type))))))

(defun format-size (size)
  (when size
    (format-char "(")
    (format-number size)
    (format-char ")")))
