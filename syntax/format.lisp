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
(defvar *command-elements*)
(defvar *binding-variables*)
(defvar *binding-types*)
(defvar *binding-values*)

(defgeneric format-sql-syntax-node (node database)
  (:documentation "Formats an SQL syntax node into *sql-stream*.")

  (:method :around (node (database null))
           (error "May not call with null DATABASE"))

  (:method (node database)
           (format-sql-literal node database)))

(defun reduce-subsequences (sequence predicate reducer)
  (iter (with completely-reduced? = #t)
        (for index :from 0 :below (length sequence))
        (for reducibles = (iter (while (< index (length sequence)))
                                (for element = (elt sequence index))
                                (while (funcall predicate element))
                                (collect element)
                                (incf index)))
        (collect (if (zerop (length reducibles))
                     (progn
                       (setf completely-reduced? #f)
                       (elt sequence index))
                     (progn
                       (decf index)
                       (apply reducer reducibles)))
          :into result
          :result-type vector)
        (finally (return (values result completely-reduced?)))))

(defun vector-extend (extension vector)
  (bind ((original-length (length vector))
         (extension-length (length extension))
         (new-length (+ original-length extension-length))
         (original-dimension (array-dimension vector 0)))
    (when (< original-dimension new-length)
      (setf vector (adjust-array vector (max (* 2 original-dimension) new-length))))
    (setf (fill-pointer vector) new-length)
    (replace vector extension :start1 original-length)
    vector))

;; TODO: if sql-quote is added this should return a lambda returning the syntax-node unless it is an sql-quote in which case it can process
(defun expand-sql-ast-into-lambda-form (syntax-node &key database (toplevel #t))
  (let ((*print-pretty* #f)
        (*print-circle* #f)
        (*sql-stream* (make-string-output-stream))
        (*database* (or database
                        (and (boundp '*database*)
                             *database*)
                        (progn
                          (simple-style-warning "Using generic database type to format constant SQL AST parts at compile time.")
                          (make-instance 'database))))
        (*command-elements* (make-array 8 :adjustable #t :fill-pointer 0))
        (*binding-variables* (make-array 16 :adjustable #t :fill-pointer 0))
        (*binding-types* (make-array 16 :adjustable #t :fill-pointer 0))
        (*binding-values* (make-array 16 :adjustable #t :fill-pointer 0)))
    (assert *database*)
    ;; TODO (?) this formatting could be put in a load-time-value and then loading the fasl's would react to
    ;; changing *database* before loading them and use the syntax customizations specified by it.
    (format-sql-syntax-node syntax-node *database*)
    (bind ((last-command-element (get-output-stream-string *sql-stream*)))
      (unless (zerop (length last-command-element))
        (vector-push-extend last-command-element *command-elements*)))
    (flet ((constant-command-element-p (element)
             (stringp element))
           (constant-variable-p (element)
             (or (null element)
                 (not (typep element 'sql-unquote))))
           (constant-type-p (element)
             (not (typep element 'sql-unquote)))
           (constant-value-p (element)
             (not (typep element 'sql-unquote)))
           (process-elements (sequence vector)
             (iter (for element :in-vector sequence)
                   (collect (if (arrayp element)
                                (unless (zerop (length element))
                                  `(vector-extend ,element ,vector))
                                `(vector-push-extend ,(form-of element) ,vector))))))
      (bind (((:values command-elements constant-command-elements?) (reduce-subsequences *command-elements* #'constant-command-element-p #'strcat))
             ((:values variables constant-variables?) (reduce-subsequences *binding-variables* #'constant-variable-p #'vector))
             ((:values types constant-types?) (reduce-subsequences *binding-types* #'constant-type-p #'vector))
             ((:values values constant-values?) (reduce-subsequences *binding-values* #'constant-value-p #'vector))
             (expand-as-constant-command-elements? (and toplevel constant-command-elements?))
             (expand-as-constant-variables? (and toplevel constant-variables? constant-command-elements?))
             (expand-as-constant-types? (and toplevel constant-types? constant-command-elements?))
             (expand-as-constant-values? (and toplevel constant-values? constant-command-elements? (every #'null *binding-variables*)))
             (body
              `(,@(unless expand-as-constant-variables?
                          (process-elements variables '*binding-variables*))
                  ,@(unless expand-as-constant-types?
                            (process-elements types '*binding-types*))
                  ,@(unless expand-as-constant-values?
                            (process-elements values '*binding-values*))
                  ,@(unless expand-as-constant-command-elements?
                            (iter (for element :in-vector command-elements)
                                  (collect (if (stringp element)
                                               `(write-string ,element *sql-stream*)
                                               element))))))
             (bindings
              (when toplevel
                `(,@(unless expand-as-constant-command-elements?
                            '((*print-pretty* #f)
                              (*print-circle* #f)
                              (*sql-stream* (make-string-output-stream))))
                    ,@(unless expand-as-constant-variables?
                              '((*binding-variables* (make-array 16 :adjustable #t :fill-pointer 0))))
                    ,@(unless expand-as-constant-types?
                              '((*binding-types* (make-array 16 :adjustable #t :fill-pointer 0))))
                    ,@(unless expand-as-constant-values?
                              '((*binding-values* (make-array 16 :adjustable #t :fill-pointer 0)))))))
             (result
              (if toplevel
                  `(values
                    ,(if expand-as-constant-command-elements? (first* command-elements) '(get-output-stream-string *sql-stream*))
                    ,(if expand-as-constant-variables? *binding-variables* '*binding-variables*)
                    ,(if expand-as-constant-types? *binding-types* '*binding-types*)
                    ,(if expand-as-constant-values? *binding-values* '*binding-values*))
                  '(values))))
        `(lambda ()
           ,@(if bindings
                 `((bind ,bindings
                     ,@body
                     ,result))
                 `(,@body
                   ,result)))))))

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
        (push-form-into-command-elements
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
                (if (spliced-p node)
                    (push-form-into-command-elements
                     `(format-separated-list ,(form-of node) ,separator ,database ',format-fn))
                    (expand-sql-unquote node database format-fn))
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
