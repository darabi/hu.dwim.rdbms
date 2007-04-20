;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms.postgresql)

#.(file-header)

(defmethod format-sql-syntax-node ((action sql-add-column-action) (database postgresql))
  (format-string "ADD ")
  (format-sql-identifier (name-of action) database)
  (format-char " ")
  (format-sql-syntax-node (type-of action) database))

(defmethod format-sql-literal ((literal array) (database postgresql))
  (format-string "E'")
  (loop for el across literal
        do (if (or (<= 0 el 31)
                   (<= 127 el 255)
                   (eq el #\')
                   (eq el #\\))
               (format *sql-stream* "\\\\~3,'0o" el)
               (format-char (code-char el))))
  (format-string "'::bytea"))

(defmethod format-sql-literal ((literal sql-literal) (database postgresql))
  (if (type-of literal)
      (progn
        (vector-push-extend literal *binding-entries*)
        (format-string "$")
        (format-string (princ-to-string (length *binding-entries*)))
        (format-string "::")
        (format-sql-syntax-node (type-of literal) database))
      (call-next-method)))

(defmethod format-sql-syntax-node ((variable sql-binding-variable) (database postgresql))
  (vector-push-extend variable *binding-entries*)
  (format-string "$")
  (format-string (princ-to-string (length *binding-entries*)))
  (awhen (type-of variable)
    (format-string "::")
    (format-sql-syntax-node (type-of variable) database)))
