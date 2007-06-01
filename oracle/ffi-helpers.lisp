;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms.oracle)

#.(file-header)

;;; helpers to access foreign stuff

(defmacro dereference-foreign-pointer (data-pointer type &optional size-pointer)
  (if (eq type :string)
      `(cffi:foreign-string-to-lisp
        (cffi:mem-ref ,data-pointer :pointer)
        (cffi:mem-ref ,size-pointer 'oci:ub-4))
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

