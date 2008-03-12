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

(defmethod make-load-form ((self sql-syntax-node) &optional env)
  (make-load-form-saving-slots self :environment env))

;;;;;;;;;;;;
;;; Fragment

(define-syntax-node sql-fragment ()
  ((sql :type string))
  (:documentation "Represents an embedded SQL string.")
  (:format-sql-syntax-node
   (format-string sql)))

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

(define-syntax-node sql-binding-variable (named-sql-syntax-node)
  ((type nil
    :type sql-type)))

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

  (:method ((literal list) database)
    (format-string "(")
    (format-comma-separated-list literal database 'format-sql-literal)
    (format-string ")"))

  (:method ((literal symbol) database)
           (format-char "'")
           ;; TODO: solve escaping
           (format-string (package-name (symbol-package literal)))
           (format-string "::")
           (format-string (symbol-name literal))
           (format-char "'"))

  ;; TODO is it ok here? maybe we should have an extra format-sql-... method for the
  ;; values block in INSERT INTO...?
  (:method ((binding sql-binding-variable) database)
    (format-sql-syntax-node binding database))

  (:method ((literal sql-literal) database)
    (format-sql-literal (if (and (null (value-of literal))
                                 (not (typep (type-of literal) 'sql-boolean-type)))
                            :null
                            (value-of literal))
                        database)))

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

;;;;;;;;;
;;; Names

(defgeneric format-sql-operator-name (name database)
  (:method ((name string) database)
           (format-string name))

  (:method ((name symbol) database)
           (format-string (string-downcase name))))

(defgeneric format-sql-function-name (name database)
  (:method ((name string) database)
           (format-string name))

  (:method ((name symbol) database)
           (format-string (string-downcase name))))

;;;;;;;;;;;
;;; Unquote

(define-syntax-node sql-unquote (sql-syntax-node)
  ((form nil)
   (spliced #f :type boolean))
  (:format-sql-syntax-node
   (expand-sql-unquote self database 'format-sql-syntax-node)))

(defun push-form-into-command-elements (form)
  (vector-push-extend (get-output-stream-string *sql-stream*) *command-elements*)
  (setf *sql-stream* (make-string-output-stream))
  (vector-push-extend form *command-elements*))

;; TODO: consider having an sql-quote node which should be checked instead of being an instance of sql-syntax-node here
;; (sql-unquote :form (sql-boolean-type))                          -> ,(sql-boolean-type)
;; (sql-unquote :form #<SQL-BOOLEAN-TYPE 1234>)                    -> ,#<SQL-BOOLEAN-TYPE 1234>
;; (sql-unquote :form (sql-quote :value #<SQL-BOOLEAN-TYPE 1234>)) -> ,'#<SQL-BOOLEAN-TYPE 1234>
(defun expand-sql-unquote (unqoute-node database formatter)
  (declare (ignore database))
  (labels ((process (node)
             (cond ((consp node)
                    (cons (process (car node))
                          (process (cdr node))))
                   ((typep node 'sql-unquote)
                    (error "In ~A sql-unquote nodes cannot be nested without intermediate literal sql-syntax-nodes" unqoute-node))
                   ((typep node 'sql-syntax-node)
                    (bind ((form (expand-sql-ast-into-lambda-form node :toplevel #f)))
                      (etypecase form
                        (string
                         `(lambda ()
                            (write-string ,form *sql-stream*)))
                        (cons form))))
                   (t node))))
    (push-form-into-command-elements
     `(,@(if (symbolp formatter)
             `(,formatter)
             `(funcall ',formatter))
         ,(process (form-of unqoute-node)) *database*))))

(defmethod format-sql-syntax-node ((thunk function) database)
  (funcall thunk))

(defun unquote-aware-format-sql-literal (literal)
  (bind ((type (type-of literal)))
    (if type
        (progn
          (vector-push-extend nil *binding-variables*)
          (vector-push-extend type *binding-types*)
          (vector-push-extend (value-of literal) *binding-values*)
          #t)
        #f)))

(defun unquote-aware-format-sql-binding-variable (variable)
  (vector-push-extend variable *binding-variables*)
  (vector-push-extend (type-of variable) *binding-types*)
  (vector-push-extend nil *binding-values*))

(defmethod format-sql-literal ((node sql-unquote) database)
  (expand-sql-unquote node database 'format-sql-literal))

(defmethod format-sql-identifier ((node sql-unquote) database)
  (expand-sql-unquote node database 'format-sql-identifier))

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
  ((variable))
  (:report (lambda (error stream)
             (format stream "The variable ~A was not bound while executing a query" (variable-of error)))))

(defmethod execute-command (database transaction (command sql-statement) &rest args &key bindings &allow-other-keys)
  (bind (((:values string binding-variables binding-types binding-values) (format-sql-to-string command)))
    (update-binding-values binding-variables binding-types binding-values bindings)
    (alexandria:remove-from-plistf args :bindings)
    (apply 'execute-command database transaction string
           :binding-types binding-types :binding-values binding-values args)))
