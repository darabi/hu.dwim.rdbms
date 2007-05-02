;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms)

#.(file-header)

;;;;;;;;;;;;;;;
;;; Syntax node

(define-syntax-node sql-syntax-node ()
  ()
  (:documentation "Base class for all kind of SQL syntax elements.")
  (:format-sql-syntax-node
   (error "No formatter method for ~A" self)))

;;;;;;;;;;;
;;; Literal

(define-syntax-node sql-literal (sql-syntax-node)
  ((value
    :type (or null boolean number string symbol))
   (type nil
    :type sql-type))
  (:documentation "Represents an SQL literal.")
  (:format-sql-syntax-node
   (format-sql-literal self)))

(deftype sql-literal* ()
  '(or null boolean string symbol number sql-literal))

(defgeneric format-sql-literal (literal database)
  (:documentation "Formats an SQL literal into *sql-stream*.")

  (:method ((literal null) database)
           (format-string "NULL"))

  (:method ((literal (eql :null)) database)
           (format-string "NULL"))

  (:method ((literal (eql t)) database)
           (format-string "true"))

  ;; TODO: what about boolean false? how to distinguish from null?
  #+nil(:method ((literal (eql nil)) database)
                (format-string "false"))

  (:method ((literal number) database)
           (format-number literal))
  
  (:method ((literal string) database)
           (format-char "'")
           ;; TODO: solve escaping
           (format-string literal)
           (format-char "'"))

  (:method ((literal symbol) database)
           (format-char "'")
           ;; TODO: solve escaping
           (format-string (package-name (symbol-package literal)))
           (format-string "::")
           (format-string (symbol-name literal))
           (format-char "'"))

  (:method ((literal list) database)
           (format-string "(")
           (format-comma-separated-list literal database)
           (format-string ")"))

  (:method ((literal sql-literal) database)
           (format-sql-literal (value-of literal) database)))

(define-syntax-node sql-binding-variable (sql-syntax-node)
  ((name
    :type (or string symbol))
   (type nil
    :type sql-type)))

(defprint-object (self sql-binding-variable)
  (princ (name-of self)))

;;;;;;;;;;;;;;
;;; Identifier

(define-syntax-node sql-identifier (sql-syntax-node)
  ((name
    :type (or string symbol)))
  (:documentation "Represents an SQL identifier.")
  (:format-sql-syntax-node
   (format-sql-identifier self)))

(deftype sql-identifier* ()
  '(or string symbol sql-identifier))

(defgeneric format-sql-identifier (identifier database)
  (:documentation "Formats an SQL identifier into *sql-stream*.")

  ;; allows to put other AST nodes in place of identifiers (e.g. table name of select statements)
  (:method (literal database)
           (format-sql-syntax-node literal database))
  
  (:method ((identifier string) database)
           (format-string identifier))

  (:method ((identifier symbol) database)
           (format-string (string-downcase identifier)))

  (:method ((identifier sql-identifier) database)
           (format-sql-identifier (name-of identifier) database)))

;;;;;;;;;;;;;
;;; Statement

(defclass* sql-statement (sql-syntax-node)
  ()
  (:documentation "Base class for all top level SQL statements which can be executed."))

(defclass* sql-ddl-statement (sql-statement)
  ())

(defclass* sql-dml-statement (sql-statement)
  ())

;;;;;;;;;;;
;;; Execute

(defcondition* unbound-binding-variable-error (rdbms-error)
  ((variable)
   (query))
  (:report (lambda (error stream)
             (format stream "The variable ~A was not bound while executing the query ~A"
                     (variable-of error) (query-of error)))))

(defmethod execute-command (database transaction (command sql-statement) &rest args &key bindings &allow-other-keys)
  (remf-keywords args :bindings)
  (collecting (final-bindings)
    (multiple-value-bind (string binding-entries) (format-sql-to-string command)
      (loop :for binding-entry :across binding-entries :do
            (assert (type-of binding-entry) ((type-of binding-entry)) "The type of literals and binding variables must be defined because they are transmitted through the binding infrastructure")
            (etypecase binding-entry
              (sql-literal
               (collect (type-of binding-entry))
               (collect (value-of binding-entry)))
              (sql-binding-variable
               (collect (type-of binding-entry))
               (aif (getf bindings (name-of binding-entry))
                    (collect it)
                    (error 'unbound-binding-variable-error :variable binding-entry :query command)))))
      (apply 'execute-command database transaction string :bindings final-bindings args))))
