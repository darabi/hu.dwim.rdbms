;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms)

#.(file-header)

(defparameter *sql-syntax-node-names* nil)

(defun import-sql-syntax-node-names (&optional (package *package*))
  (import *sql-syntax-node-names* package))

(defvar *sql-stream*)

;;;;;;;;;;;;;;;
;;; Syntax node

(defclass* sql-syntax-node ()
  ()
  (:documentation "Base class for all kind of SQL syntax elements."))

(defgeneric format-sql-syntax-node (node database)
  (:documentation "Formats an SQL syntax node into *sql-stream*.")

  (:method (node database)
           (format-sql-literal node database)))

(defun format-sql (statement &key (stream t) (database *database*))
  "Formats the given SQL statement into the stream."
  (let ((*sql-stream* stream)
        (*database* database))
    (format-sql-syntax-node statement database)
    (values)))

(defun format-sql-to-string (statement &rest args &key &allow-other-keys)
  "Formats the given SQL statement into a string."
  (with-output-to-string (stream)
    (apply #'format-sql statement :stream stream args)))

(defmacro define-syntax-node (name supers slots &rest options)
  `(progn
    (defclass* ,name ,supers ,slots
               ,@(remove-if (lambda (option)
                              (starts-with (string-downcase (first option)) "format"))
                            options))
    (pushnew ',name *sql-syntax-node-names*)
    ,(awhen (find :format-sql-syntax-node options :key #'first)
            `(defmethod format-sql-syntax-node ((self ,name) database)
              (macrolet ((format-sql-syntax-node (node)
                           `(funcall 'format-sql-syntax-node ,node database))
                         (format-sql-identifier (node)
                           `(funcall 'format-sql-identifier ,node database)))
                (with-slots ,(mapcar #'first slots) self
                  ,@(rest it)))))
    ,(awhen (find :format-sql-identifier options :key #'first)
            `(defmethod format-sql-identifier ((self ,name) database)
              (macrolet ((format-sql-syntax-node (node)
                           `(funcall 'format-sql-syntax-node ,node database))
                         (format-sql-identifier (node)
                           `(funcall 'format-sql-identifier ,node database)))
                (with-slots ,(mapcar #'first slots) self
                  ,@(rest it)))))
    (find-class ',name)))

(defmacro format-comma-separated-identifiers (nodes)
  `(format-comma-separated-list ,nodes nil format-sql-identifier))

(defmacro format-comma-separated-list (nodes &optional database (format-fn 'format-sql-syntax-node))
  `(loop for i = nil then t
    for node in ,nodes
    when i
    do (write-string ", " *sql-stream*)
    do ,(if database
            `(,format-fn node ,database)
            `(,format-fn node))))

(defmacro format-string (string)
  `(write-string ,string *sql-stream*))

(defmacro format-char (character)
  `(write-char
    ,(if (typep character 'string)
         (progn
           (assert (= (length character) 1) nil "format-char must be called with a character or a 1 character long string")
           (elt character 0))
         character) *sql-stream*))

(defmacro format-number (number)
  `(write ,number :stream *sql-stream*))

;;;;;;;;;;;
;;; Literal

(defclass* sql-literal (sql-syntax-node)
  ((value
    :type (or null boolean number string symbol)))
  (:documentation "Represents an SQL literal."))

(deftype sql-literal* ()
  '(or null boolean string symbol number sql-literal))

(defgeneric format-sql-literal (literal database)
  (:documentation "Formats an SQL literal into *sql-stream*.")

  (:method (literal database)
           (format-sql-syntax-node literal database))

  (:method ((literal null) database)
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
           (format-string literal)
           (format-char "'"))

  (:method ((literal symbol) database)
           (format-char "'")
           (format-string (package-name (symbol-package literal)))
           (format-string "::")
           (format-string (symbol-name literal))
           (format-char "'"))

  (:method ((literal sql-literal) database)
           (format-sql-literal (value-of literal) database)))

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

(defmacro format-where (where &optional database)
  `(when ,where
    (format-string " WHERE ")
    ,(if database
         `(format-sql-syntax-node ,where ,database)
         `(format-sql-syntax-node ,where))))

;;;;;;;;;;;
;;; Execute

(defmethod execute-command :around (database transaction (command sql-statement) &optional visitor)
  (execute-command database transaction (format-sql-to-string command) visitor))

(defmethod execute-command :before (database transaction (command sql-ddl-statement) &optional visitor)
  (declare (ignore visitor))
  (unless (ddl-only-p *transaction*)
    (error 'transaction-error
           :format-control "DDL statements are not allowed to be executed within a transaction, because they implicitly commit")))
