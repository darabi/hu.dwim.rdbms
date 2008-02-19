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
(defvar *binding-types*)
(defvar *binding-values*)

(defgeneric format-sql-syntax-node (node database)
  (:documentation "Formats an SQL syntax node into *sql-stream*.")

  (:method :around (node (database null))
           (error "May not call with null DATABASE"))

  (:method (node database)
           (format-sql-literal node database)))

(defun expand-sql-ast-into-lambda-form (syntax-node &key database)
  (let ((*sql-stream* (make-string-output-stream))
        (*sql-stream-elements* (make-array 8 :adjustable #t :fill-pointer 0))
        (*database* (or database
                        (and (boundp '*database*)
                             *database*)
                        (progn
                          (simple-style-warning "Using generic database type to format constant SQL AST parts at compile time.")
                          (make-instance 'database))))
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
      (if (and (zerop (length *binding-values*))
               (every #'stringp *sql-stream-elements*))
          (apply #'concatenate 'string (append (coerce *sql-stream-elements* 'list)
                                               (list (get-output-stream-string *sql-stream*))))
          `(lambda ()
            (let ((*binding-types* ,(copy-array *binding-types*))
                  (*binding-values* ,(copy-array *binding-values*)))
              ,@(iter (for element :in-vector *sql-stream-elements*)
                      (collect (if (stringp element)
                                   (unless (zerop (length element))
                                     `(write-string ,element *sql-stream*))
                                   element)))
              ,(let ((last-chunk (get-output-stream-string *sql-stream*)))
                    (unless (zerop (length last-chunk))
                      `(write-string ,last-chunk *sql-stream*)))
              (values *binding-types* *binding-values*)))))))

(defmethod execute-command :around (database transaction (command function) &rest args &key bindings &allow-other-keys)
  (let* (binding-types
         binding-values
         (command (with-output-to-string (*sql-stream*)
                                         (setf (values binding-types binding-values) (funcall command)))))
    (update-binding-types-and-values binding-types binding-values bindings)
    (apply #'execute-command database transaction command
           :binding-types binding-types :binding-values binding-values  args)))

(defun format-sql (syntax-node &key (stream t) (database *database*))
  "Formats the given SQL syntax node into the stream."
  (let* ((*print-pretty* #f)
         (*sql-stream* stream)
         (*database* database)
         (*binding-types* (make-array 16 :adjustable #t :fill-pointer 0))
         (*binding-values* (make-array 16 :adjustable #t :fill-pointer 0)))
    (format-sql-syntax-node syntax-node database)
    (values stream *binding-types* *binding-values*)))

(defun format-sql-to-string (syntax-node &rest args &key &allow-other-keys)
  "Formats the given SQL syntax node into a string."
  (let* ((*print-pretty* #f)
         scratch
         binding-types
         binding-values
         (string (with-output-to-string (stream)
                   (setf (values scratch binding-types binding-values)
                         (apply #'format-sql syntax-node :stream stream args)))))
    (values string binding-types binding-values)))

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
              `(defmethod format-sql-syntax-node ((self ,name) database)
                (macrolet ((format-sql-syntax-node (node)
                             `(funcall 'format-sql-syntax-node ,node database))
                           (format-sql-literal (node)
                             `(funcall 'format-sql-literal ,node database))
                           (format-sql-identifier (node)
                             `(funcall 'format-sql-identifier ,node database)))
                  (with-slots ,effective-slots self
                    ,@(rest it)))))
      ,(awhen (find :format-sql-identifier options :key #'first)
              `(defmethod format-sql-identifier ((self ,name) database)
                (macrolet ((format-sql-syntax-node (node)
                             `(funcall 'format-sql-syntax-node ,node database))
                           (format-sql-literal (node)
                             `(funcall 'format-sql-literal ,node database))
                           (format-sql-identifier (node)
                             `(funcall 'format-sql-identifier ,node database)))
                  (with-slots ,effective-slots
                      self
                    ,@(rest it)))))
      (pushnew ',name *sql-constructor-names*)
      (defmacro ,name (&body args)
        `(make-instance ',',name ,@args))
      (find-class ',name))))

(defmacro format-comma-separated-identifiers (nodes)
  `(format-comma-separated-list ,nodes nil format-sql-identifier))

(defmacro format-comma-separated-list (nodes &optional database (format-fn 'format-sql-syntax-node))
  (with-unique-names (node)
    (rebinding (nodes)
      `(if (sql-unquote-p ,nodes)
           (push-form-into-sql-stream-elements
            `(iter (for ,',node :in-sequence ,(form-of ,nodes))
                   (unless (first-iteration-p)
                     (write-string ", " *sql-stream*))
                   (,',format-fn ,',node *database*)))
        (iter (for ,node :in-sequence ,nodes)
              (unless (first-iteration-p)
                (write-string ", " *sql-stream*))
              ,(if database
                   `(,format-fn ,node ,database)
                   `(,format-fn ,node)))))))

(defmacro format-separated-list (nodes separator &optional database (format-fn 'format-sql-syntax-node))
  `(iter (for node :in-sequence ,nodes)
         (unless (first-iteration-p)
           (write-string " " *sql-stream*)
           (write-string ,separator *sql-stream*)
           (write-string " " *sql-stream*))
         ,(if database
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

(defun print-number-to-sql-string (number)
  (etypecase number
    (integer (princ-to-string number))
    (ratio (format nil "~F" (coerce number 'double-float)))
    (float (format nil "~F" number))))

(defmacro format-number (number)
  (rebinding (number)
    `(write-string (print-number-to-sql-string ,number) *sql-stream*)))

(defmacro format-where (where &optional database)
  (rebinding (where)
    `(when ,where
      (format-string " WHERE ")
      ,(if database
           `(format-sql-syntax-node ,where ,database)
           `(format-sql-syntax-node ,where)))))
