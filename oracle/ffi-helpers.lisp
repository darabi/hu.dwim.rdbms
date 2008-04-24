;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms.oracle)

(define-symbol-macro null (cffi:null-pointer))

(defun make-void-pointer ()
  (cffi:foreign-alloc '(:pointer :void) :initial-element null))

(defparameter *default-oci-flags* (logior oci:+threaded+ oci:+new-length-semantics+))

;;; helpers to access foreign stuff

(defmacro dereference-foreign-pointer (data-pointer type &optional size-pointer)
  (if (eq type :string)
      `(cffi:foreign-string-to-lisp
        (cffi:mem-ref ,data-pointer :pointer)
        :count (cffi:mem-ref ,size-pointer 'oci:ub-4)
        :encoding (connection-encoding-of (database-of *transaction*)))
      `(cffi:mem-ref ,data-pointer ,type)))

(defun oci-attr-get (param-descriptor
                     attribute-id
                     attribute-value         ; output
                     attribute-value-length) ; output
  (oci-call (oci:attr-get param-descriptor
                          oci:+dtype-param+
                          attribute-value
                          attribute-value-length
                          attribute-id
                          (error-handle-of *transaction*))))

(defmacro get-param-descriptor-attribute (param-descriptor attribute type)
  `(cffi:with-foreign-objects ((data-pointer ,type)
                               (size-pointer 'oci:ub-4))
    (oci-call (oci:attr-get ,param-descriptor
               oci:+dtype-param+
               data-pointer
               size-pointer
               ,attribute
               (error-handle-of *transaction*)))
    (dereference-foreign-pointer data-pointer ,type size-pointer)))

(defmacro get-statement-attribute (statement attribute type)
  `(cffi:with-foreign-objects ((data-pointer ,type)
                               (size-pointer 'oci:ub-4))
    (oci-call (oci:attr-get (statement-handle-of ,statement)
               oci:+htype-stmt+
               data-pointer
               size-pointer
               ,attribute
               (error-handle-of *transaction*)))
    (dereference-foreign-pointer data-pointer ,type size-pointer)))

(defmacro get-row-count-attribute (statement)
  `(get-statement-attribute ,statement oci:+attr-row-count+ 'oci:ub-4))

(defmacro with-foreign-oci-string ((string c-string c-size &key (null-terminated-p #f)) &body body)
  `(cffi:with-foreign-string (,c-string ,string :byte-size-variable ,c-size
                              :encoding (connection-encoding-of (database-of *transaction*))
                              :null-terminated-p ,null-terminated-p)
    ,@body))

(defmacro foreign-oci-string-alloc (string &rest args)
  `(cffi:foreign-string-alloc
    ,string
    :encoding (connection-encoding-of (database-of *transaction*))
    ,@args))

(defun oci-string-to-lisp (pointer &optional size)
  (cffi:foreign-string-to-lisp pointer :count size
                               :encoding (connection-encoding-of (database-of *transaction*))))

(defun oci-char-width ()
  (cffi::null-terminator-len (connection-encoding-of (database-of *transaction*)))) ;; FIXME using internal fn

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

(defun stmt-execute (statement mode)
  (oci-call (oci:stmt-execute (service-context-handle-of *transaction*)
                              (statement-handle-of statement)
                              (error-handle-of *transaction*)
                              (if (select-p statement) 0 1)
                              0
                              null
                              null
                              mode)))

(defun stmt-fetch-2 (statement number-of-rows orientation offset)
  (let ((status (oci:stmt-fetch-2 (statement-handle-of statement)
                                  (error-handle-of *transaction*)
                                  number-of-rows
                                  orientation
                                  offset
                                  *default-oci-flags*)))
    (case status
      (#.oci:+success+ #t)
      (#.oci:+no-data+ #f)
      (t (handle-oci-error)))))

(defun stmt-fetch-last (statement)
  (stmt-fetch-2 statement 1 oci:+fetch-last+ 0))

(defun stmt-fetch-next (statement number-of-rows)
  (stmt-fetch-2 statement number-of-rows oci:+fetch-next+ 0))

(defun handle-alloc (handle-ptr handle-type)
  (oci-call (oci:handle-alloc (environment-handle-of *transaction*)
                              handle-ptr
                              handle-type
                              0
                              null)))

(defun dump-c-byte-array (ptr size)
  (with-output-to-string (s)
                         (loop for i from 0 below size
                               do (format s "~2,'0X "
                                          (cffi:mem-ref ptr :uint8 i)))))

(defun descriptor-alloc (descriptor-ptr-ptr descriptor-type)
  (oci-call (oci:descriptor-alloc (environment-handle-of *transaction*)
                                  descriptor-ptr-ptr
                                  descriptor-type
                                  0
                                  null)))

(defun allocate-oci-date-time (descriptor-ptr-ptr)
  (descriptor-alloc descriptor-ptr-ptr oci:+dtype-timestamp+))

(defun allocate-oci-date-time-tz (descriptor-ptr-ptr)
  (descriptor-alloc descriptor-ptr-ptr oci:+dtype-timestamp-tz+))


(defun descriptor-free (descriptor-ptr descriptor-type)
  (oci-call (oci:descriptor-free descriptor-ptr
                                 descriptor-type)))

(defun free-oci-date-time (descriptor-ptr)
  (descriptor-free descriptor-ptr oci:+dtype-timestamp+))

(defun free-oci-date-time-tz (descriptor-ptr)
  (descriptor-free descriptor-ptr oci:+dtype-timestamp-tz+))



