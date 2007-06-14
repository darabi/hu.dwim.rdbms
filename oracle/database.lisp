;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms.oracle)

#.(file-header)

(defclass* oracle (database)
  ())

(defmethod transaction-mixin-class list ((db oracle))
  'oracle-transaction)

(defclass* oracle-transaction (transaction)
  ((environment-handle-pointer nil :accessor environment-handle-pointer)
   (error-handle nil :accessor error-handle-pointer)
   (server-handle nil :accessor server-handle-pointer)
   (service-context-handle nil :accessor service-context-handle-pointer)
   (session-handle nil :accessor session-handle-pointer)))

(macrolet ((def (&rest names)
               `(progn
                 ,@(loop for name in names
                         collect `(defun ,(concatenate-symbol name "-of") (transaction)
                                   (cffi:mem-ref (,(concatenate-symbol name "-pointer") transaction)
                                    '(:pointer :void)))))))
  (def
    environment-handle
    error-handle
    server-handle
    session-handle
    service-context-handle))

(defclass* oracle-prepared-statement (prepared-statement)
  ((statement-handle-pointer nil :accessor statement-handle-pointer)
   (bindings nil :type list)
   (select #f :type boolean)))

(defun statement-handle-of (statement)
  (cffi:mem-ref (statement-handle-pointer statement) '(:pointer :void)))


