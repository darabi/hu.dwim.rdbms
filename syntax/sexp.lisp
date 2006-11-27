;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms)

#.(file-header)

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
       (equalp (string a) (string b))))

(defun compile-sql (form)
  (let* ((sql-compiler-function-name (concatenate-symbol "compile-sql-" (first form) (find-package :cl-rdbms))))
    (if (fboundp sql-compiler-function-name)
        (apply sql-compiler-function-name (rest form))
        (sql-compile-error form))))

(defun process-sql-syntax-list (visitor body &key function-call-allowed-p)
  (if (and function-call-allowed-p
           (sql-function-call-form-p body))
      (list (compile-sql-function-call body))
      (if (consp body)
          (loop for node :in body
                collect (if (and function-call-allowed-p
                                 (sql-function-call-form-p node))
                            (compile-sql-function-call node)
                            (process-sql-syntax-node visitor node)))
          (list (process-sql-syntax-node visitor body)))))

(defun process-sql-syntax-node (visitor node)
  (if (typep node 'sql-syntax-node)
      node
      (funcall visitor node)))

(defun compile-sql-select (columns tables &optional where)
  (make-instance 'sql-select
                 :columns (process-sql-syntax-list #'compile-sql-column-alias columns :function-call-allowed-p #t)
                 :tables (process-sql-syntax-list #'compile-sql-table-alias tables)
                 :where where))

(defun compile-sql-delete (table &optional where)
  (make-instance 'sql-delete
                 :table (process-sql-syntax-list #'compile-sql-table-alias table)
                 :where where))

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
                 :arguments (mapcar #'compile-sql-function-call-argument (rest body))))

(defun compile-sql-function-call-argument (body)
  (if (sql-symbol-equal body "*")
      (make-instance 'sql-all-columns)
      (make-instance 'sql-identifier :name (string body))))

(defun compile-sql-column-alias (body)
  (cond ((sql-symbol-equal body "*")
         (make-instance 'sql-all-columns))
        ;; TODO this is temporary
        ((and (consp body)
              (sql-function-name-p (first body)))
         (compile-sql-function-call body))
        (t (let ((name)
                 (table)
                 (column)
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
                    (setf table (subseq name 0 it))
                    (setf column (subseq name (1+ it))))
                  (setf column name))
             (make-instance 'sql-column-alias :table table :column column :alias alias)))))

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
