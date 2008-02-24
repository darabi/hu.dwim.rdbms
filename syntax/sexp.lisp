;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

;;; This file contains the sexp-sql -> sql-ast parser. (and is far from being done)

(in-package :cl-rdbms)

#.(file-header)

;; TODO support [select (count *) !(some lisp generating the from part)] syntax

(defmacro sql (body)
  "Parse BODY as an sexp-sql sexp."
  (expand-sql-ast-into-lambda-form
   (compile-sexp-sql body)))

(defcondition* sql-compile-error (error)
  ((whole-form)
   (error-at-form))
  (:report (lambda (c stream)
             (let ((*print-circle* nil))
               (format stream "Error while compiling sql form ~S~:[~; at form ~S~]."
                       (whole-form-of c)
                       (error-at-form-of c)
                       (error-at-form-of c))))))

(defun sql-compile-error (whole-form &optional error-at-form)
  (error 'sql-compile-error :whole-form whole-form :error-at-form error-at-form))

(defun sql-symbol-equal (a b)
  (and (or (symbolp a) (stringp a))
       (or (symbolp b) (stringp b))
       (equalp (string a) (string b))))

(defun compile-sexp-sql (form)
  (assert (consp form))
  (let ((first (first form)))
    (cond
      ((sql-symbol-equal first 'select) (compile-sexp-sql-select form))
      ((sql-symbol-equal first 'insert) (compile-sexp-sql-insert form))
      ((sql-symbol-equal first 'update) (compile-sexp-sql-update form))
      ((sql-symbol-equal first 'delete) (compile-sexp-sql-delete form))
      ((sql-symbol-equal first 'create) (compile-sexp-sql-create form))
      ((sql-symbol-equal first 'drop)   (compile-sexp-sql-drop form))
      (t (sql-compile-error form)))))

(defun process-sexp-sql-syntax-list (body visitor &key function-call-allowed-p)
  (if (and function-call-allowed-p
           (sql-function-call-form-p body))
      (list (compile-sexp-sql-function-call body))
      (cond
        ((sql-unquote-p body)
         (compile-sexp-sql-unquote body))
        ((consp body)
         (loop for node :in body
               collect (cond
                         ((typep node 'sql-syntax-node)
                          node)
                         ((and function-call-allowed-p
                               (sql-function-call-form-p node))
                          (compile-sexp-sql-function-call node))
                         (t (process-sexp-sql-syntax-node node visitor)))))
        (t (list (process-sexp-sql-syntax-node body visitor))))))

(defun process-sexp-sql-syntax-node (node &optional (visitor #'identity))
  (cond ((typep node 'sql-syntax-node)
         node)
        ((sql-unquote-p node)
         (funcall visitor (compile-sexp-sql-unquote node)))
        (t (funcall visitor node))))

(defun sql-function-name-p (thing)
  (let ((name (cond ((and thing (symbolp thing))
                     (string-downcase thing))
                    ((stringp thing)
                     thing))))
    ;; TODO
    (string= name "count")))

(defun stringify (name)
  (typecase name
    (string name)
    (symbol (string-downcase name))
    (t (error "Don't know how to stringify ~S" name))))

(defun sql-function-call-form-p (thing)
  (and (consp thing)
       (sql-function-name-p (first thing))))

(defun sql-unquote-p (thing)
  (or (typep thing 'sql-unquote)
      (and (consp thing)
           (sql-symbol-equal (first thing) 'sql-unquote))))

(defun compile-sexp-sql-unquote (body)
  (if (typep body 'sql-unquote)
      body
      (progn
        (unless (<= 2 (length body) 3)
          (sql-compile-error body))
        (make-instance 'sql-unquote :form (second body) :spliced (third body)))))

(defun compile-sexp-sql-select (body)
  (pop body)
  (destructuring-bind (columns tables &optional where) body
    (make-instance 'sql-select
                   :columns (process-sexp-sql-syntax-list columns #'compile-sexp-sql-column-alias :function-call-allowed-p #t)
                   :tables (process-sexp-sql-syntax-list tables #'compile-sexp-sql-table-alias)
                   ;; TODO process where
                   :where (process-sexp-sql-syntax-node where))))

(defun compile-sexp-sql-create (body)
  (let ((whole-body body)
        (what (progn
                (pop body)
                (pop body))))
    (cond ((sql-symbol-equal what "table")
           (destructuring-bind (&key temporary) (when (listp (first body))
                                                  (pop body))
             (make-instance 'sql-create-table
                            :temporary temporary
                            :name (pop body)
                            :columns (process-sexp-sql-syntax-list (pop body) #'compile-sexp-sql-column))))
          (t (sql-compile-error whole-body body)))))

(defun compile-sexp-sql-drop (body)
  (let ((whole-body body)
        (what (progn
                (pop body)
                (pop body))))
    (cond ((sql-symbol-equal what "table")
           (make-instance 'sql-drop-table
                          :name (pop body)))
          (t (sql-compile-error whole-body body)))))

(defun compile-sexp-sql-insert (body)
  (let ((whole-body body))
    (pop body)
    (prog1
        (make-instance 'sql-insert
                       :table (process-sexp-sql-syntax-node (pop body))
                       :columns (process-sexp-sql-syntax-list (pop body) #'compile-sexp-sql-column)
                       :values (process-sexp-sql-syntax-list (pop body) #'compile-sexp-sql-literal))
      (when body
        (sql-compile-error whole-body body)))))

(defun compile-sexp-sql-delete (body)
  (pop body)
  (bind (((table &optional where) body))
    (make-instance 'sql-delete
                   :table (process-sexp-sql-syntax-node table #'compile-sexp-sql-table-alias)
                   ;; TODO process where
                   :where (process-sexp-sql-syntax-node where))))

(defun compile-sexp-sql-update (body)
  (error "Not yet implemented"))

(defun compile-sexp-sql-function-call (body)
  (make-instance 'sql-function-call
                 :name (string-downcase (first body))
                 :arguments (mapcar #'compile-sexp-sql-function-call-argument (rest body))))

(defun compile-sexp-sql-function-call-argument (body)
  (if (sql-symbol-equal body "*")
      (make-instance 'sql-all-columns)
      (make-instance 'sql-identifier :name (string body))))

(defun compile-sexp-sql-column-alias (body)
  (cond
    ((sql-unquote-p body)
     (compile-sexp-sql-unquote body))
    ((sql-symbol-equal body "*")
     (make-instance 'sql-all-columns))
    ;; TODO this is temporary
    ((and (consp body)
          (sql-function-name-p (first body)))
     (compile-sexp-sql-function-call body))
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
         (setf name (stringify name))
         (aif (position #\. name)
              (progn
                (setf table (subseq name 0 it))
                (setf column (subseq name (1+ it))))
              (setf column name))
         (make-instance 'sql-column-alias :table table :column column :alias alias)))))

(defun compile-sexp-sql-literal (body)
  (let ((whole-body body)
        (value)
        (type))
    (if (consp body)
        (if (sql-symbol-equal (first body) '?)
            (progn
              (pop body)
              (return-from compile-sexp-sql-literal (compile-sexp-sql-binding-variable body)))
            (progn
              (setf value (process-sexp-sql-syntax-node (pop body)))
              (setf type (compile-sexp-sql-type (pop body)))
              (when body
                (sql-compile-error whole-body body))))
        (setf value body))
    (make-instance 'sql-literal :value (process-sexp-sql-syntax-node value) :type type)))

(defun compile-sexp-sql-binding-variable (body)
  (make-instance 'sql-binding-variable :name (pop body) :type (compile-sexp-sql-type (pop body))))

(defun compile-sexp-sql-type (body)
  (cond
    ((typep body 'sql-syntax-node)
     body)
    ((sql-unquote-p body)
     (compile-sexp-sql-unquote body))
    (t
     (let ((name (if (consp body)
                     (first body)
                     body))
           (type-args (when (consp body)
                        (rest body))))
       (cond ((sql-symbol-equal name "char")
              (make-instance 'sql-character-type :size (when type-args
                                                         (first type-args))))
             ((sql-symbol-equal name "varchar")
              (make-instance 'sql-character-varying-type :size (when type-args
                                                                 (first type-args))))
             ((sql-symbol-equal name "integer")
              (make-instance 'sql-integer-type :bit-size (when type-args
                                                           (first type-args))))
             ((sql-symbol-equal name "float")
              (make-instance 'sql-float-type :bit-size (when type-args
                                                         (first type-args))))
             ((sql-symbol-equal name "numeric")
              (make-instance 'sql-numeric-type))
             ((or (sql-symbol-equal name "boolean")
                  (sql-symbol-equal name "bool"))
              (make-instance 'sql-boolean-type))
             ((sql-symbol-equal name "date")
              (make-instance 'sql-date-type))
             ((sql-symbol-equal name "time")
              (make-instance 'sql-time-type))
             ((sql-symbol-equal name "timestamp")
              (make-instance 'sql-timestamp-type :with-timezone (when type-args
                                                                  (first type-args))))
             ((sql-symbol-equal name "clob")
              (make-instance 'sql-character-large-object-type :size (when type-args
                                                                      (first type-args))))
             ((sql-symbol-equal name "blob")
              (make-instance 'sql-binary-large-object-type :size (when type-args
                                                                   (first type-args))))
             (t (sql-compile-error body)))))))

(defun compile-sexp-sql-column (body)
  (let ((name)
        (type))
    (if (consp body)
        (progn
          (assert (<= (length body) 2))
          (setf name (first body))
          (setf type (compile-sexp-sql-type (second body))))
        (progn
          (setf name body)))
    (make-instance 'sql-column :name name :type type)))

(defun compile-sexp-sql-columns (body)
  (process-sexp-sql-syntax-list body #'compile-sexp-sql-column))

(defun compile-sexp-sql-table-alias (body)
  (let ((whole-body body)
        (name)
        (alias))
    (if (consp body)
        (progn
          (setf name (pop body))
          (setf alias (pop body))
          (when body
            (sql-compile-error whole-body body)))
        (progn
          (setf name body)))
    (make-instance 'sql-table-alias :name name :alias alias)))
