;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms.oracle)

#.(file-header)

(define-symbol-macro null (cffi:null-pointer))

(defun make-void-pointer ()
  (cffi:foreign-alloc '(:pointer :void)))

(defparameter *default-oci-flags* (logior oci:+threaded+ oci:+new-length-semantics+))

;;; helpers to access foreign stuff

(defmacro dereference-foreign-pointer (data-pointer type &optional size-pointer)
  (if (eq type :string)
      `(cffi:foreign-string-to-lisp
        (cffi:mem-ref ,data-pointer :pointer)
        :count (cffi:mem-ref ,size-pointer 'oci:ub-4)
        :encoding (connection-encoding-of (database-of *transaction*)))
      `(cffi:mem-ref ,data-pointer ,type)))

(defmacro get-param-descriptor-attribute (param-descriptor attribute type)
  `(cffi:with-foreign-objects ((data-pointer ,type)
                               (size-pointer 'oci:ub-4))
    (oci-call (oci:attr-get ,param-descriptor
               oci:+dtype-param+
               data-pointer
               size-pointer
               ,attribute
               (error-handle-of transaction)))
    (dereference-foreign-pointer data-pointer ,type size-pointer)))

(defmacro get-statement-attribute (statement attribute type)
  `(cffi:with-foreign-objects ((data-pointer ,type)
                               (size-pointer 'oci:ub-4))
    (oci-call (oci:attr-get (statement-handle-of ,statement)
               oci:+htype-stmt+
               data-pointer
               size-pointer
               ,attribute
               (error-handle-of transaction)))
    (dereference-foreign-pointer data-pointer ,type size-pointer)))

(defmacro with-foreign-oci-string ((string c-string c-size) &body body)
  `(cffi:with-foreign-string (,c-string ,string :byte-size-variable ,c-size
                              :encoding (connection-encoding-of (database-of *transaction*))
                              :null-terminated-p #f)
    ,@body))

(defun oci-string-to-lisp (pointer &optional size)
  (cffi:foreign-string-to-lisp pointer :count size
                               :encoding (connection-encoding-of (database-of *transaction*))))

(defun set-session-string-attribute (attribute value)
  (with-foreign-oci-string (value c-string c-size)
    (oci-call (oci:attr-set (session-handle-of *transaction*)
                            oci:+htype-session+
                            c-string
                            c-size
                            attribute
                            (error-handle-of *transaction*)))))

(defun server-attach (datasource)
  (with-foreign-oci-string (datasource c-datasource c-size)
    (oci-call (oci:server-attach (server-handle-of *transaction*)
                                 (error-handle-of *transaction*)
                                 c-datasource
                                 c-size
                                 oci:+default+))))

(defun stmt-prepare (statement command)
  (with-foreign-oci-string (command c-command c-size)
    (oci-call (oci:stmt-prepare (statement-handle-of statement)
                                (error-handle-of *transaction*)
                                c-command
                                c-size
                                oci:+ntv-syntax+
                                *default-oci-flags*))))

(defun handle-alloc (handle-ptr handle-type)
  (oci-call (oci:handle-alloc (environment-handle-of *transaction*)
                              handle-ptr
                              handle-type
                              0
                              null)))


