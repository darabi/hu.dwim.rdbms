;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms)

#.(file-header)

(defparameter *sql-syntax-node-names* nil)

(defparameter *sql-constructor-names* nil)

(defun import-sql-syntax-node-names (&optional (package *package*))
  (import *sql-syntax-node-names* package))

(defun import-sql-constructor-names (&optional (package *package*))
  (import *sql-constructor-names* package))

(defvar *sql-stream*)
(defvar *sql-stream-elements*)
(defvar *binding-variables*)
(defvar *binding-types*)
(defvar *binding-values*)

(defgeneric format-sql-syntax-node (node database)
  (:documentation "Formats an SQL syntax node into *sql-stream*.")

  (:method :around (node (database null))
           (error "May not call with null DATABASE"))

  (:method (node database)
           (format-sql-literal node database)))

;; TODO: if sql-quote is added this should return a lambda returning the syntax-node unless it is an sql-quote in which case it can process
(defun expand-sql-ast-into-lambda-form (syntax-node &key database (toplevel #t))
  (let ((*print-pretty* #f)
        (*print-circle* #f)
        (*sql-stream* (make-string-output-stream))
        (*sql-stream-elements* (make-array 8 :adjustable #t :fill-pointer 0))
        (*database* (or database
                        (and (boundp '*database*)
                             *database*)
                        (progn
                          (simple-style-warning "Using generic database type to format constant SQL AST parts at compile time.")
                          (make-instance 'database))))
        (*binding-variables* (make-array 16 :adjustable #t :fill-pointer 0))
        (*binding-types* (make-array 16 :adjustable #t :fill-pointer 0))
        (*binding-values* (make-array 16 :adjustable #t :fill-pointer 0)))
    (assert *database*)
    ;; TODO (?) this formatting could be put in a load-time-value and then loading the fasl's would react to
    ;; changing *database* before loading them and use the syntax customizations specified by it.
    (format-sql-syntax-node syntax-node *database*)
    (flet ((copy-array (array)
             (let ((length (length array)))
               (if (and (zerop length)
                        (every 'stringp *sql-stream-elements*))
                   #()
                   `(make-array ,length :adjustable #t :fill-pointer ,length
                                :initial-contents ,array)))))
      (bind ((strings-only? (every #'stringp *sql-stream-elements*))
             (command
              (when strings-only?
                (apply #'concatenate 'string (append (coerce *sql-stream-elements* 'list)
                                                     (list (get-output-stream-string *sql-stream*)))))))
        (if (and strings-only?
                 (every #'null *binding-variables*))
            (if (zerop (length *binding-types*))
                command
                `(lambda ()
                   (values ,command ,*binding-variables* ,*binding-types* ,*binding-values*)))
            (let ((body
                   `(,@(iter (for element :in-vector *sql-stream-elements*)
                             (collect (if (stringp element)
                                          (unless (zerop (length element))
                                            `(write-string ,element *sql-stream*))
                                          element)))
                       ,(let ((last-chunk (get-output-stream-string *sql-stream*)))
                             (unless (zerop (length last-chunk))
                               `(write-string ,last-chunk *sql-stream*)))
                       ,(if toplevel
                            `(values ,(if strings-only?
                                          command
                                          '(get-output-stream-string *sql-stream*))
                                     ,(if strings-only?
                                          *binding-variables*
                                          '*binding-variables*)
                                     ,(if strings-only?
                                          *binding-types*
                                          '*binding-types*)
                                     ,(if (and strings-only?
                                               (zerop (length *binding-variables*)))
                                          *binding-values*
                                          '*binding-values*))
                            '(values)))))
              (if toplevel
                  `(lambda ()
                     (bind (,@(unless strings-only?
                                      `((*print-pretty* #f)
                                        (*print-circle* #f)
                                        (*sql-stream* (make-string-output-stream))))
                              ,@(unless strings-only?
                                        `((*binding-variables* ,(copy-array *binding-variables*))))
                              ,@(unless strings-only?
                                        `((*binding-types* ,(copy-array *binding-types*))))
                              ,@(unless (and strings-only?
                                             (zerop (length *binding-variables*)))
                                        `((*binding-values* ,(copy-array *binding-values*)))))
                       ,@body))
                  `(lambda ()
                     ,@body))))))))

(defmethod execute-command :around (database transaction (command function) &rest args &key bindings &allow-other-keys)
  (bind (((:values command binding-variables binding-types binding-values) (funcall command)))
    (update-binding-values binding-variables binding-types binding-values bindings)
    (alexandria:remove-from-plistf args :bindings)
    (apply #'execute-command database transaction command
           :binding-types binding-types :binding-values binding-values args)))

(defun format-sql (syntax-node &key (stream t) (database *database*))
  "Formats the given SQL syntax node into the stream."
  (let* ((*print-pretty* #f)
         (*sql-stream* stream)
         (*database* database)
         (*binding-variables* (make-array 16 :adjustable #t :fill-pointer 0))
         (*binding-types* (make-array 16 :adjustable #t :fill-pointer 0))
         (*binding-values* (make-array 16 :adjustable #t :fill-pointer 0)))
    (format-sql-syntax-node syntax-node database)
    (values stream *binding-variables* *binding-types* *binding-values*)))

(defun format-sql-to-string (syntax-node &rest args &key &allow-other-keys)
  "Formats the given SQL syntax node into a string."
  (let* ((*print-pretty* #f)
         scratch
         binding-variables
         binding-types
         binding-values
         (string (with-output-to-string (stream)
                   (setf (values scratch binding-variables binding-types binding-values)
                         (apply #'format-sql syntax-node :stream stream args)))))
    (values string binding-variables binding-types binding-values)))

(defun sql-constructor-name (name)
  (concatenate-symbol (find-package :cl-rdbms) "SQL-" name))

(defun sql-operator-name (name)
 (if (every (lambda (char) (char= char #\-)) (symbol-name name))
     (string-upcase name)
     (substitute #\Space #\- (string-upcase name))))

(defmacro define-syntax-node (name supers slots &rest options)
  (let ((effective-slots (delete-duplicates
                          (append (mapcan (lambda (super) (copy-list (get super :slot-names))) supers)
                                  (mapcar #'first slots)))))
    (flet ((define-format-method (method-name body)
             `(defmethod ,method-name ((self ,name) database)
                (macrolet ((format-sql-syntax-node (node)
                             `(funcall 'format-sql-syntax-node ,node database))
                           (format-sql-literal (node)
                             `(funcall 'format-sql-literal ,node database))
                           (format-sql-identifier (node)
                             `(funcall 'format-sql-identifier ,node database))
                           (format-comma-separated-list (nodes &optional format-fn)
                             `(funcall 'format-comma-separated-list ,nodes database
                                       ,@(when format-fn
                                               (list format-fn))))
                           (format-comma-separated-identifiers (nodes)
                             `(funcall 'format-comma-separated-identifiers ,nodes database))
                           (format-separated-list (nodes separator &optional format-fn)
                             `(funcall 'format-separated-list ,nodes ,separator database
                                       ,@(when format-fn
                                               (list format-fn))))
                           (format-sql-where (expression)
                             `(funcall 'format-sql-where ,expression database)))
                  (with-slots ,effective-slots self
                    ,@body)))))
      `(progn
         (eval-always
           (setf (get ',name :slot-names) ',effective-slots))
         (defclass* ,name ,supers ,slots
           ,@(remove-if (lambda (option)
                          (starts-with (string-downcase (first option)) "format"))
                        options))
         (defmethod make-load-form ((self ,name) &optional env)
           (make-load-form-saving-slots self :environment env))
         (pushnew ',name *sql-syntax-node-names*)
         ,(awhen (find :format-sql-syntax-node options :key #'first)
                 (define-format-method 'format-sql-syntax-node (rest it)))
         ,(awhen (find :format-sql-identifier options :key #'first)
                 (define-format-method 'format-sql-identifier (rest it)))
         (pushnew ',name *sql-constructor-names*)
         (defmacro ,name (&body args)
           `(make-instance ',',name ,@args))
         (find-class ',name)))))

(def function format-comma-separated-list (nodes database &optional (format-fn 'format-sql-syntax-node))
  (format-separated-list nodes #\, database format-fn))

(def function format-comma-separated-identifiers (nodes database)
  (format-comma-separated-list nodes database 'format-sql-identifier))

(def function format-separated-list (nodes separator database &optional (format-fn 'format-sql-syntax-node))
  (if (typep nodes 'sql-unquote)
      (progn
        (assert (not (spliced-p nodes)))
        (push-form-into-sql-stream-elements
         `(format-separated-list ,(form-of nodes) ,separator ,database ',format-fn)))
      (iter (for node :in-sequence nodes)
            (unless (first-iteration-p)
              (unless (eq #\, separator)
                (write-char #\Space *sql-stream*))
              (if (typep separator 'character)
                  (write-char separator *sql-stream*)
                  (write-string separator *sql-stream*))
              (write-char #\Space *sql-stream*))
            (if (typep node 'sql-unquote)
                (push-form-into-sql-stream-elements
                 (if (spliced-p node)
                     `(format-separated-list ,(form-of node) ,separator ,database ',format-fn)
                     (expand-sql-unquote node database format-fn)))
                (funcall format-fn node database)))))

(defmacro format-char (character)
  `(write-char
    ,(if (typep character 'string)
         (progn
           (assert (= (length character) 1) nil "format-char must be called with a character or a 1 character long string")
           (elt character 0))
         character) *sql-stream*))

(defmacro format-string (string)
  `(write-string ,string *sql-stream*))

(def function print-number-to-sql-string (number)
  (etypecase number
    (integer (princ-to-string number))
    (ratio (format nil "~F" (coerce number 'double-float)))
    (float (format nil "~F" number))))

(defmacro format-number (number)
  `(write-string (print-number-to-sql-string ,number) *sql-stream*))

(def function format-sql-where (where database)
  (when where
    (format-string " WHERE ")
    (format-sql-syntax-node where database)))
