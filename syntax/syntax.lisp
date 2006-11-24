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

(defclass* sql-syntax-node ()
  ()
  (:documentation "Base class for all kind of SQL syntax elements."))

(defclass* sql-statement (sql-syntax-node)
  ()
  (:documentation "Base class for all top level SQL statements which can be executed."))

(defclass* sql-ddl-statement (sql-statement)
  ())

(defclass* sql-dml-statement (sql-statement)
  ())

(defmacro define-syntax-node (name supers slots &rest options)
  `(progn
    (defclass* ,name ,supers ,slots
               ,@options)
    (pushnew ',name *sql-syntax-node-names*)))

(defun format-sql (statement &key (stream t) (database *database*))
  (let ((*sql-stream* stream)
        (*database* database))
    (format-sql-syntax-node statement database)
    (values)))

(defun format-sql-to-string (statement &rest args &key &allow-other-keys)
  (with-output-to-string (stream)
    (apply #'format-sql statement :stream stream args)))

(defgeneric format-sql-syntax-node (node database)
  (:documentation "Formats an SQL syntax node into *sql-stream*.")

  (:method ((s string) database)
           (write-string s *sql-stream*))

  (:method ((i integer) database)
           (write i :stream *sql-stream*)))

(defmethod execute-command :around (database transaction (command sql-statement) &optional visitor)
  (execute-command database transaction (format-sql-to-string command) visitor))

(defmethod execute-command :before (database transaction (command sql-ddl-statement) &optional visitor)
  (declare (ignore visitor))
  (error 'transaction-error
         :format-control "DDL statements are not allowed to be executed within a transaction, because they implicitly commit"))

(defmacro sql (body)
  "Evaluate BODY and parse the result as an sql sexp."
  `(compile-sql ,body))

(defmacro sql* (body)
  "Same as SQL, but does not evaluate BODY."
  `(compile-sql ',body))

(defun sql-compile-error (form &optional error-at-form)
  (error "Error while compiling sql form ~S at form ~S" form error-at-form))

(defun sql-symbol-equal (a b)
  (and (or (symbolp a) (stringp a))
       (or (symbolp b) (stringp b))
       (equalp (string-downcase (string a))
               (string-downcase (string b)))))

(defun compile-sql (form)
  (cond
    ((sql-symbol-equal (first form) "select")
     (compile-sql-select (rest form)))
    (t (sql-compile-error form))))

(defun process-sql-syntax-list (visitor body &key function-call-allowed-p)
  (if (and function-call-allowed-p
           (sql-function-call-form-p body))
      (list (compile-sql-function-call body))
      (if (consp body)
          (loop for node :in body
                collect (if (and function-call-allowed-p
                                 (sql-function-call-form-p node))
                            (compile-sql-function-call node)
                            (process-sql-syntax-node visitor body)))
          (list (process-sql-syntax-node visitor body)))))

(defun process-sql-syntax-node (visitor node)
  (if (typep node 'sql-syntax-node)
      node
      (funcall visitor node)))

(defun compile-sql-select (body)
  (make-instance 'sql-select
                 :column-aliases (process-sql-syntax-list #'compile-sql-column-alias (first body)
                                                          :function-call-allowed-p #t)
                 :table-aliases (process-sql-syntax-list #'compile-sql-table-alias (second body))
                 :where (third body)))

(defun compile-sql-symbol (symbol)
  (if (and symbol (symbolp symbol))
      (string-downcase symbol)
      symbol))

(defun sql-function-name-p (thing)
  (let ((name (cond ((and thing (symbolp thing))
                     (string-downcase thing))
                    ((stringp thing)
                     thing))))
    ;; TODO
    (string= name "count")))

(defun sql-function-call-form-p (thing)
  (and (consp thing)
       (sql-function-name-p (first thing))))

(defun compile-sql-function-call (body)
  (make-instance 'sql-function-call
                 :name (string-downcase (first body))
                 :arguments (mapcar #'string (rest body))))

(defun compile-sql-column-alias (body)
  (cond ((sql-symbol-equal body "*")
         (make-instance 'sql-all-columns))
        ;; TODO this is temporary
        ((and (consp body)
              (sql-function-name-p (first body)))
         (compile-sql-function-call body))
        (t (let ((name)
                 (table-name)
                 (column-name)
                 (alias))
             (if (consp body)
                 (progn
                   (setf name (first body))
                   (setf alias (second body)))
                 (progn
                   (setf name body)))
             (setf name (compile-sql-symbol name))
             (setf alias (compile-sql-symbol alias))
             (aif (position #\. name)
                  (progn
                    (setf table-name (subseq name 0 it))
                    (setf column-name (subseq name (1+ it))))
                  (setf column-name name))
             (make-instance 'sql-column-alias :table-name table-name :column-name column-name :alias alias)))))

(defun compile-sql-table-alias (body)
  (let ((name)
        (alias))
    (if (consp body)
        (progn
          (setf name (first body))
          (setf alias (second body)))
        (progn
          (setf name body)))
    (setf name (compile-sql-symbol name))
    (setf alias (compile-sql-symbol alias))
    (make-instance 'sql-table-alias :name name :alias alias)))






