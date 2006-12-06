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
  (let* ((sql-compiler-function-name (concatenate 'string
                                                  (symbol-name '#:compile-sql-)
                                                  (when (symbolp (first form))
                                                    (symbol-name (first form)))))
         (handler-name (find-symbol sql-compiler-function-name (find-package :cl-rdbms))))
    (if (and handler-name
             (fboundp handler-name))
        (funcall handler-name (rest form))
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

(defun compile-sql-select (body)
  (destructuring-bind (columns tables &optional where) body
    (make-instance 'sql-select
                   :columns (process-sql-syntax-list #'compile-sql-column-alias columns :function-call-allowed-p #t)
                   :tables (process-sql-syntax-list #'compile-sql-table-alias tables)
                   :where where)))

(defun compile-sql-create (body)
  (let ((whole-body body)
        (what (pop body)))
    (cond ((sql-symbol-equal what "table")
           (destructuring-bind (&key temporary) (when (listp (first body))
                                                  (pop body))
             (make-instance 'sql-create-table
                            :temporary temporary
                            :name (pop body)
                            :columns (process-sql-syntax-list #'compile-sql-column (pop body)))))
          (t (sql-compile-error whole-body body)))))

(defun compile-sql-insert (body)
  (let ((whole-body body))
    (prog1
        (make-instance 'sql-insert
                       :table (pop body)
                       :columns (process-sql-syntax-list #'compile-sql-column (pop body))
                       :values (process-sql-syntax-list #'compile-sql-literal (pop body)))
      (when body
        (sql-compile-error whole-body body)))))

(defun compile-sql-delete (body)
  (destructuring-bind (table &optional where) body
    (make-instance 'sql-delete
                   :table (process-sql-syntax-list #'compile-sql-table-alias table)
                   :where where)))

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

(defun stringify (name)
  (typecase name
    (string name)
    (symbol (string-downcase name))
    (t (error "Don't know how to stringify ~S" name))))

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
             (setf name (stringify name))
             (aif (position #\. name)
                  (progn
                    (setf table (subseq name 0 it))
                    (setf column (subseq name (1+ it))))
                  (setf column name))
             (make-instance 'sql-column-alias :table table :column column :alias alias)))))

(defun compile-sql-literal (body)
  (let ((whole-body body)
        (value)
        (type))
    (if (consp body)
        (if (sql-symbol-equal (first body) '?)
            (progn
              (pop body)
              (return-from compile-sql-literal (compile-sql-binding-variable body)))
            (progn
              (setf value (pop body))
              (setf type (compile-sql-type (pop body)))
              (when body
                (sql-compile-error whole-body body))))
        (setf value body))
    (make-instance 'sql-literal :value value :type type)))

(defun compile-sql-binding-variable (body)
  (make-instance 'sql-binding-variable :name (pop body) :type (compile-sql-type (pop body))))

(defun compile-sql-type (body)
  (let ((name (if (consp body)
                  (first body)
                  body))
        (type-args (when (consp body)
                     (rest body))))
    (cond ((sql-symbol-equal name "varchar")
           (make-instance 'sql-character-varying-type :size (when type-args
                                                              (first type-args))))
          ((sql-symbol-equal name "integer")
           (make-instance 'sql-integer-type :bit-size (when type-args
                                                        (first type-args))))
          (t (sql-compile-error body)))))

(defun compile-sql-column (body)
  (let ((name)
        (type))
    (if (consp body)
        (progn
          (assert (<= (length body) 2))
          (setf name (first body))
          (setf type (compile-sql-type (second body))))
        (progn
          (setf name body)))
    (make-instance 'sql-column :name name :type type)))

(defun compile-sql-columns (body)
  (process-sql-syntax-list #'compile-sql-column body))

(defun compile-sql-table-alias (body)
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
