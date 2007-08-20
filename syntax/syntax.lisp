;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms)

#.(file-header)

;;;;;;;;;;;;;;;
;;; Syntax node

(define-syntax-node named-sql-syntax-node (sql-syntax-node)
  ((name nil
    :type sql-identifier*)))

(defprint-object (self named-sql-syntax-node)
  (princ (name-of self)))

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
           (format-string "FALSE"))

  (:method ((literal (eql :null)) database)
           (format-string "NULL"))

  (:method ((literal (eql t)) database)
           (format-string "TRUE"))

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

(define-syntax-node sql-binding-variable (named-sql-syntax-node)
  ((type nil
    :type sql-type)))

;;;;;;;;;;;;;;
;;; Identifier

(define-syntax-node sql-identifier (named-sql-syntax-node)
  ()
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
           (format-sql-identifier (string-downcase identifier) database))

  (:method ((identifier sql-identifier) database)
           (format-sql-identifier (name-of identifier) database)))

;;;
;;; Names
;;;
(defgeneric format-sql-name (name)
  (:method ((name string))
           (format-string name))

  (:method ((name symbol))
           (format-string (string-downcase name))))


;;;;;;;;;;;;;
;;; Backquote

(define-syntax-node sql-backquote (sql-syntax-node)
  ((form nil))
  (:format-sql-syntax-node
   (expand-sql-backquote self 'format-sql-syntax-node)))

(defun expand-sql-backquote (node formatter)
  (vector-push-extend (get-output-stream-string *sql-stream*) *sql-stream-elements*)
  (vector-push-extend `(,formatter ,(form-of node) *database*) *sql-stream-elements*)
  (setf *sql-stream* (make-string-output-stream)))

(defmethod format-sql-literal :before ((literal sql-literal) database)
  (assert (not (typep (value-of literal) 'sql-backquote)) () "SQL-BACKQUOTE is not allowed in SQL-LITERAL"))

(defmethod format-sql-literal ((node sql-backquote) database)
  (expand-sql-backquote node 'format-sql-literal))

(defmethod format-sql-identifier ((node sql-backquote) database)
  (expand-sql-backquote node 'format-sql-identifier))

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
  (multiple-value-bind (string binding-entries) (format-sql-to-string command)
    (let ((final-bindings
           (iter (for binding-entry :in-vector binding-entries)
                 (assert (type-of binding-entry) ((type-of binding-entry)) "The type of literals and binding variables must be defined because they are transmitted through the binding infrastructure")
                 (etypecase binding-entry
                   (sql-literal
                    (collect (type-of binding-entry))
                    (collect (value-of binding-entry)))
                   (sql-binding-variable
                    (collect (type-of binding-entry))
                    (aif (getf bindings (name-of binding-entry))
                         (collect it)
                         (error 'unbound-binding-variable-error :variable binding-entry :query command)))))))
      (apply 'execute-command database transaction string :bindings final-bindings args))))
