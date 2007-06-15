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


(defun oci-call (code)
  (declare (cl:type fixnum code))
  (handle-oci-result code))

(defun handle-oci-result (result)
  (case result
    (#.oci:+success+
     result)
    (#.oci:+error+
     (handle-oci-error))
    (#.oci:+no-data+
     (simple-rdbms-error "OCI No data found"))
    (#.oci:+success-with-info+
     (simple-rdbms-error "Internal error: unexpected oci:+success-with-info+"))
    (#.oci:+invalid-handle+
     (simple-rdbms-error "OCI Invalid handle"))
    (#.oci:+need-data+
     (simple-rdbms-error "OCI Need data"))
    (#.oci:+still-executing+
     (error 'sql-temporary-error :format-control "OCI Still Executing"))
    (#.oci:+continue+
     (simple-rdbms-error "OCI Continue"))
    (1804
     (simple-rdbms-error "Check ORACLE_HOME and NLS settings"))
    (t
     (simple-rdbms-error "Unknown OCI error, code is ~A" result))))

(defun handle-oci-error ()
  (unless (error-handle-pointer *transaction*)
    (simple-rdbms-error "OCI error in initialization stage, too early to query the actual error"))
  (cffi:with-foreign-objects ((error-code 'oci:sb-4))
    (cffi:with-foreign-pointer (error-buffer oci:+error-maxmsg-size+)
      (setf (cffi:mem-ref error-buffer :char) 0)
      (setf (cffi:mem-ref error-code 'oci:sb-4) 0)

      (oci:error-get (error-handle-of *transaction*) 1
                     (cffi:null-pointer)
                     error-code
                     error-buffer
                     oci:+error-maxmsg-size+ oci:+htype-error+)

      (let ((error-message (cffi:foreign-string-to-lisp error-buffer)))
        (log.error "Signalling error: ~A" error-message)
        (simple-rdbms-error "RDBMS error: ~A" error-message)))))
