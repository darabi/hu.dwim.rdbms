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
                         (format-sql-literal (node)
                           `(funcall 'format-sql-literal ,node database))
                         (format-sql-identifier (node)
                           `(funcall 'format-sql-identifier ,node database)))
                (with-slots ,(mapcar #'first slots) self
                  ,@(rest it)))))
    ,(awhen (find :format-sql-identifier options :key #'first)
            `(defmethod format-sql-identifier ((self ,name) database)
              (macrolet ((format-sql-syntax-node (node)
                           `(funcall 'format-sql-syntax-node ,node database))
                         (format-sql-literal (node)
                           `(funcall 'format-sql-literal ,node database))
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

(defmacro format-separated-list (nodes separator &optional database (format-fn 'format-sql-syntax-node))
  `(loop for i = nil then t
    for node in ,nodes
    when i
    do (write-string ,separator *sql-stream*)
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
