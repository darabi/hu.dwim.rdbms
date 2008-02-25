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

  (:method ((literal sql-literal) database)
    (format-sql-literal (if (and (null (value-of literal))
                                 (not (typep (type-of literal) 'sql-boolean-type)))
                            :null
                            (value-of literal))
                        database)))

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
   (expand-sql-unquote self 'format-sql-syntax-node)))

(defun push-form-into-sql-stream-elements (form &optional (flush? #t))
  (when flush?
    (vector-push-extend (get-output-stream-string *sql-stream*) *sql-stream-elements*)
    (setf *sql-stream* (make-string-output-stream)))
  (vector-push-extend form *sql-stream-elements*))

(defun expand-sql-unquote (node formatter)
  (labels ((process (node)
             (cond ((consp node)
                    (cons (process (car node))
                          (process (cdr node))))
                   ((typep node 'sql-syntax-node)
                    (bind ((form (expand-sql-ast-into-lambda-form node :toplevel #f)))
                      (etypecase form
                        (string
                         `(lambda ()
                            (write-string ,form *sql-stream*)))
                        (cons form))))
                   (t node))))
    (push-form-into-sql-stream-elements `(,formatter ,(process (form-of node)) *database*))))

(defmethod format-sql-syntax-node ((thunk function) database)
  (funcall thunk))

(defun unquote-aware-format-sql-literal (literal thunk call-next-method)
  (let ((type (type-of literal))
        (value (value-of literal)))
    (if type
        (if (typep value 'sql-unquote)
            (progn
              (vector-push-extend type *binding-types*)
              (vector-push-extend nil *binding-values*)
              (push-form-into-sql-stream-elements
               `(setf (aref *binding-values* ,(1- (length *binding-types*))) ,(form-of value)) #f)
              (funcall thunk))
            (progn
              (vector-push-extend type *binding-types*)
              (vector-push-extend value *binding-values*)
              (funcall thunk)))
        (funcall call-next-method))))

(defmethod format-sql-literal ((node sql-unquote) database)
  (expand-sql-unquote node 'format-sql-literal))

(defmethod format-sql-identifier ((node sql-unquote) database)
  (expand-sql-unquote node 'format-sql-identifier))

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
             (format stream "The variable ~A was not bound while executing the query ~A"
                     (variable-of error) (query-of error)))))

(defmethod execute-command (database transaction (command sql-statement) &rest args &key bindings &allow-other-keys)
  (multiple-value-bind (string binding-types binding-values)
      (format-sql-to-string command)
    (update-binding-types-and-values binding-types binding-values bindings)
    (apply 'execute-command database transaction string
           :binding-types binding-types :binding-values binding-values args)))
