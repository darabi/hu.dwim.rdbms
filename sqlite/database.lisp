;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

;;;; TODO: finish sqlite backend
;;;; TODO: implement proper error handling
(in-package :cl-rdbms.sqlite)

#.(file-header)

(defclass* sqlite-transaction (transaction)
  ((connection-pointer nil)))

(defmethod transaction-mixin-class list ((db sqlite))
  'sqlite-transaction)

(defclass* sqlite-prepared-statement (prepared-statement)
  ((statement-pointer nil)))

(defconstant +maximum-rdbms-name-length+ 30)

;; this name mapping is not injective, different lisp names _may_ be mapped to the same rdbms name
(defmethod calculate-rdbms-name ((db sqlite) thing name)
  (calculate-rdbms-name-with-utf-8-length-limit name +maximum-rdbms-name-length+ :prefix "_"))

(defun process-error (tr message &rest args)
  (apply 'process-error-code (sqlite3-cffi-bindings:sqlite-3-errcode (connection-pointer-of tr))
         (sqlite3-cffi-bindings:sqlite-3-errmsg (connection-pointer-of tr))
         message args))

(defun process-error-code (error-code error-message message &rest args)
  (unless (= sqlite3-cffi-bindings:+sqlite-ok+ error-code)
    (apply 'error (concatenate 'string message "~%Error Code: ~A, Error Message: ~A")
           (append args (list error-code error-message)))))

(defun ensure-connected (tr)
  (unless (connection-pointer-of tr)
    (bind ((connection-pointer (cffi:foreign-alloc :pointer)))
      (sqlite3-cffi-bindings:sqlite-3-open (getf (connection-specification-of (database-of tr)) :file-name) connection-pointer)
      (setf (connection-pointer-of tr) (cffi:mem-ref connection-pointer :pointer))
      (cffi:foreign-free connection-pointer)
      (process-error tr "Error during opening database"))))

(defmethod prepare-command ((db sqlite) (tr sqlite-transaction) (command string) &key &allow-other-keys)
  (ensure-connected tr)
  (bind ((statement-pointer (cffi:foreign-alloc :pointer))
         (garbage (cffi:foreign-alloc :pointer)))
    (cffi:with-foreign-string (foreign-command command)
      (sqlite3-cffi-bindings:sqlite-3-prepare (connection-pointer-of tr) foreign-command -1 statement-pointer garbage))
    (cffi:foreign-free garbage)
    (make-instance 'sqlite-prepared-statement :statement-pointer statement-pointer)))

(defvar *execute-command-result*)

(cffi:defcallback sqlite-3-vector-result-collector :int ((callback-data :pointer) (column-count :int) (row-data :pointer) (columns :pointer))
  (declare (ignore callback-data columns))
  (bind ((row (make-array column-count)))
    (loop for i :from 0 :below column-count
       do (setf (aref row i)
                (cffi:mem-ref (cffi:inc-pointer row-data (* i (cffi:foreign-type-size :pointer))) :string)))
    (vector-push-extend row *execute-command-result*)
    sqlite3-cffi-bindings:+sqlite-ok+))

(cffi:defcallback sqlite-3-list-result-collector :int ((callback-data :pointer) (column-count :int) (row-data :pointer) (columns :pointer))
  (declare (ignore callback-data columns))
  (push (loop for i :from 0 :below column-count
           collect (cffi:mem-ref (cffi:inc-pointer row-data (* i (cffi:foreign-type-size :pointer))) :string))
        *execute-command-result*)
  sqlite3-cffi-bindings:+sqlite-ok+)

(defmethod execute-command ((db sqlite) (tr sqlite-transaction) (command string) &key result-type binding-types binding-values &allow-other-keys)
  (when (or binding-types binding-values)
    (error "Binding is not yet implemented"))
  (ensure-connected tr)
  (bind ((error-pointer (cffi:foreign-alloc :pointer))
         (*execute-command-result* (ecase result-type
                                     (vector (make-array 8 :adjustable t :fill-pointer 0))
                                     (list nil))))
    (cffi:with-foreign-string (foreign-command command)
      (sqlite3-cffi-bindings:sqlite-3-exec (connection-pointer-of tr) foreign-command
                                           (ecase result-type
                                             (vector (cffi:callback sqlite-3-vector-result-collector))
                                             (list (cffi:callback sqlite-3-list-result-collector)))
                                           (cffi:null-pointer) error-pointer)
      (process-error tr "Error ~A during executing ~S" (cffi:mem-ref error-pointer :string) command)
      (cffi:foreign-free error-pointer)
      (when (eq result-type 'list)
        (setf *execute-command-result* (nreverse *execute-command-result*)))
      *execute-command-result*)))

(defmethod execute-command ((db sqlite) (tr sqlite-transaction) (prepared-statement prepared-statement) &key &allow-other-keys)
  (sqlite3-cffi-bindings:sqlite-3-step (statement-pointer-of prepared-statement)))

(defmethod cleanup-transaction ((tr sqlite-transaction))
  (awhen (connection-pointer-of tr)
    (process-error-code (sqlite3-cffi-bindings:sqlite-3-close it) nil "Error during closing database")))
