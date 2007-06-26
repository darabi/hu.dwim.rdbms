;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms.oracle)

#.(file-header)

(publish-backend-symbol 'oracle)

(defmethod begin-transaction ((database oracle) (transaction oracle-transaction))
  ;; nop, because oracle implicitly has transactions
  )

(defmethod commit-transaction ((database oracle) (transaction oracle-transaction))
  (oci-call (oci:trans-commit (service-context-handle-of transaction)
                              (error-handle-of transaction)
                              *default-oci-flags*)))

(defmethod rollback-transaction ((database oracle) (transaction oracle-transaction))
  (oci-call (oci:trans-rollback (service-context-handle-of transaction)
                                (error-handle-of transaction)
                                *default-oci-flags*)))

(defmethod prepare-command ((database oracle)
                            (transaction oracle-transaction)
                            (command string)
                            &key &allow-other-keys)
  (ensure-connected transaction)
  (log.debug "Preparing command: ~S" command)
  (make-prepared-statemenet transaction command))

(defmethod execute-command ((database oracle)
                            (transaction oracle-transaction)
                            (command string)
                            &key visitor bindings &allow-other-keys)
  (log.debug "Executing ~S" command)
  (let ((statement (prepare-command database transaction command)))
    (unwind-protect
         (execute-prepared-statement transaction statement bindings visitor)
      (free-prepared-statement statement))))

(defmethod execute-command ((database oracle)
                            (transaction oracle-transaction)
                            (prepared-statement prepared-statement)
                            &key visitor bindings &allow-other-keys)
  (execute-prepared-statement transaction prepared-statement bindings visitor))

(defmethod cleanup-transaction ((transaction oracle-transaction))
  (when (environment-handle-pointer transaction)
    (log.debug "Cleaning up Oracle transaction ~A to database ~A" transaction (database-of transaction))
    (disconnect transaction)))

;;;
;;; the actual implementation
;;;

(defmacro ignore-errors* (&body body)
  `(block nil
    (handler-bind ((serious-condition
                    (lambda (error)
                      (log.warn "Ignoring error: ~A" error)
                      (return))))
      ,@body)))

(defun ensure-connected (transaction)
  (when (cl:null (environment-handle-pointer transaction))
    (connect transaction)))

(defun connect (transaction)
  (assert (cl:null (environment-handle-pointer transaction)))

  (destructuring-bind (&key datasource user-name (password ""))
      (connection-specification-of (database-of transaction))
    (macrolet ((alloc (&rest whats)
                 `(progn
                   ,@(loop for what :in whats
                           for accessor = (concatenate-symbol what "-pointer")
                           collect `(setf (,accessor transaction)
                                     (make-void-pointer))))))
      (alloc
       environment-handle
       error-handle
       server-handle
       service-context-handle
       session-handle))

    (log.debug "Connecting in transaction ~A" transaction)
    (oci-call (oci:env-create (environment-handle-pointer transaction)
                              (logior
                               (ecase (connection-encoding-of (database-of *transaction*))
                                 (:ascii 0)
                                 (:utf-16 oci:+utf-16+))
                               *default-oci-flags*)
                              null null null null 0 null))

    (handle-alloc (error-handle-pointer transaction) oci:+htype-error+)
    (handle-alloc (server-handle-pointer transaction) oci:+htype-server+)
    (handle-alloc (service-context-handle-pointer transaction) oci:+htype-svcctx+)

    (log.debug "Logging on in transaction ~A" transaction)
    (server-attach datasource)

    (oci-call (oci:attr-set (service-context-handle-of transaction)
                            oci:+htype-svcctx+
                            (server-handle-of transaction)
                            0
                            oci:+attr-server+
                            (error-handle-of transaction)))
            
    (handle-alloc (session-handle-pointer transaction) oci:+htype-session+)

    (set-session-string-attribute oci:+attr-username+ user-name)
    (set-session-string-attribute oci:+attr-password+ password)

    (oci-call (oci:session-begin (service-context-handle-of transaction)
                                 (error-handle-of transaction)
                                 (session-handle-of transaction)
                                 oci:+cred-rdbms+
                                 oci:+default+))

    (oci-call (oci:attr-set (service-context-handle-of transaction)
                            oci:+htype-svcctx+
                            (session-handle-of transaction)
                            0
                            oci:+attr-session+
                            (error-handle-of transaction)))))

(defun disconnect (transaction)
  (assert (environment-handle-pointer transaction))
  
  (ignore-errors*
    (log.debug "Calling logoff in transaction ~A" transaction)
    (oci-call (oci:logoff (service-context-handle-of transaction)
                          (error-handle-of transaction))))
  (ignore-errors*
    (log.debug "Freeing environment handle of transaction ~A" transaction)
    (oci-call (oci:handle-free (environment-handle-of transaction) oci:+htype-env+)))
  
  (macrolet ((dealloc (&rest whats)
               `(progn
                 ,@(loop for what in whats
                         for accessor = (concatenate-symbol what "-pointer")
                         collect `(awhen (,accessor transaction)
                                   (cffi:foreign-free it)
                                   (setf (,accessor transaction) nil))))))
    (dealloc
     environment-handle
     error-handle
     server-handle
     service-context-handle
     session-handle)))

(defun make-prepared-statemenet (transaction command &optional (name ""))
  (let ((statement (make-instance 'oracle-prepared-statement
                                  :name name
                                  :statement-handle-pointer (make-void-pointer)
                                  :query command)))
    (handle-alloc (statement-handle-pointer statement) oci:+htype-stmt+)
    (stmt-prepare statement command)
    (log.dribble "Statement is allocated")
    (setf (select-p statement) (= (get-statement-attribute
                                   statement
                                   oci:+attr-stmt-type+
                                   'oci:ub-2)
                                  oci:+stmt-select+))
    statement))

(defun free-prepared-statement (statement)
  (oci-call (oci:handle-free (statement-handle-of statement) oci:+htype-stmt+))
  (free-bindings (bindings-of statement))
  (cffi:foreign-free (statement-handle-pointer statement)))

(defun execute-prepared-statement (transaction statement bindings visitor)
  (let ((iters (if (select-p statement) 0 1)))

    ;; make bindings
    (setf (bindings-of statement) (make-bindings statement transaction bindings))
    
    ;; execute
    (oci-call (oci:stmt-execute (service-context-handle-of transaction)
                                (statement-handle-of statement)
                                (error-handle-of transaction)
                                iters
                                0
                                null
                                null
                                *default-oci-flags*)))
  
  (cond
    ((select-p statement)
     (let ((cursor (make-oracle-cursor statement transaction)))
       (unwind-protect
            (if visitor
                (loop for row = (fetch-row cursor)
                      while row
                      do (funcall visitor row))
                (loop for row = (fetch-row cursor)
                      while row
                      collect row))
         (free-cursor cursor))))
    (t
     nil)))


(defclass* oracle-binding ()
  ((bind-handle-pointer)
   (sql-type)
   (typemap)
   (data-pointer)
   (data-size)
   (indicator)))

(defun make-bindings (statement transaction bindings)
  (loop for (type value) :on bindings :by #'cddr
        for position :from 1
        collect (make-binding statement transaction position type value)))

(defun make-binding (statement transaction position sql-type value)
  (let* ((statement-handle (statement-handle-of statement))
         (error-handle (error-handle-of transaction))
         (typemap (typemap-for-sql-type sql-type))
         (oci-type-code (typemap-external-type typemap))
         (converter (typemap-lisp-to-oci typemap))
         (bind-handle-pointer (cffi:foreign-alloc :pointer :initial-element null))
         (indicator (cffi:foreign-alloc 'oci:sb-2 :initial-element (if (eq value :null) -1 0)))) 
    (multiple-value-bind (data-pointer data-size)
        (if (eql value :null)
            (values null 0)
            (funcall converter value))

      (log.dribble "Value ~S converted to ~A" value (dump-c-byte-array data-pointer data-size))
      
      (oci-call (oci:bind-by-pos statement-handle
                                 bind-handle-pointer
                                 error-handle
                                 position
                                 data-pointer
                                 data-size
                                 oci-type-code
                                 indicator
                                 null               ; alenp
                                 null               ; rcodep
                                 0                  ; maxarr_len
                                 null               ; curelep
                                 *default-oci-flags*))
      (make-instance 'oracle-binding
                     :bind-handle-pointer bind-handle-pointer
                     :sql-type sql-type
                     :typemap typemap
                     :data-pointer data-pointer
                     :data-size data-size
                     :indicator indicator))))

(defun free-bindings (bindings)
  (mapc 'free-binding bindings))

(defun free-binding (binding)
  (cffi:foreign-free (bind-handle-pointer-of binding))
  (cffi:foreign-free (indicator-of binding))
  (let ((data-pointer (data-pointer-of binding)))
    (unless (cffi:null-pointer-p data-pointer)
      (cffi:foreign-free data-pointer))))


(defconstant +number-of-buffered-rows+ 200)

(defclass* oracle-cursor ()
  ((statement)
   (transaction)
   (column-descriptors)
   (cumulative-row-count 0)
   (buffered-row-count 0)
   (current-row-index 0)
   (end-seen #f :type boolean)))

(defclass* column-descriptor ()
  ((define-handle-pointer)
   (name)
   (size)
   (buffer)
   (typemap)
   (indicators)
   (return-codes)))

(defun make-oracle-cursor (statement transaction)
  (make-instance 'oracle-cursor
                 :statement statement
                 :transaction transaction
                 :column-descriptors (make-column-descriptors statement transaction)))

(defun make-column-descriptors (statement transaction)
  (coerce (cffi:with-foreign-objects ((param-descriptor-pointer :pointer))
            (loop for column-index from 1  ; OCI 1-based indexing
                  while (eql (oci:param-get (statement-handle-of statement)
                                            oci:+htype-stmt+
                                            (error-handle-of transaction)
                                            param-descriptor-pointer
                                            column-index)
                             oci:+success+)
                  collect (make-column-descriptor statement
                                                  transaction
                                                  column-index
                                                  (cffi:mem-ref param-descriptor-pointer :pointer))))
          'simple-vector))

(defun oci-attr-get (param-descriptor
                     attribute-id
                     attribute-value        ; output
                     attribute-value-length ; output
                     error-handle)
  "TODO"
  (oci-call (oci:attr-get param-descriptor
                          oci:+dtype-param+
                          attribute-value
                          attribute-value-length
                          attribute-id
                          error-handle)))

(defun make-column-descriptor (statement transaction position param-descriptor)
  (cffi:with-foreign-objects ((attribute-value :uint8 8) ; 8 byte buffer for attribute values
                              (attribute-value-length 'oci:ub-4))
    (flet ((oci-attr-get (attribute-id cffi-type)
             (oci-attr-get param-descriptor attribute-id attribute-value
                           attribute-value-length (error-handle-of transaction))
             (cffi:mem-ref attribute-value cffi-type))
           (oci-string-attr-get (attribute-id)
             (oci-attr-get param-descriptor attribute-id attribute-value
                           attribute-value-length (error-handle-of transaction))
             (oci-string-to-lisp
              (cffi:mem-ref attribute-value '(:pointer :unsigned-char)) ; OraText*
              (cffi:mem-ref attribute-value-length 'oci:ub-4))))
              
            
      (let ((column-name (oci-string-attr-get oci:+attr-name+))
            (column-type (oci-attr-get oci:+attr-data-type+ 'oci:ub-2))
            (column-size)
            (precision)
            (scale)
            (typemap)
            (define-handle-pointer (cffi:foreign-alloc :pointer :initial-element null))
            (return-codes (cffi:foreign-alloc :unsigned-short :count +number-of-buffered-rows+))
            (indicators (cffi:foreign-alloc :short :count +number-of-buffered-rows+)))
        (declare (fixnum column-type))

        (progn
          ; KLUDGE oci:+attr-data-size+ returned as ub-2, despite it is documented as ub-4
          (setf (cffi:mem-ref attribute-value :unsigned-short) 0)
          (setf column-size (oci-attr-get oci:+attr-data-size+ 'oci:ub-2)))

        (log.dribble "Retrieving column: name=~W, type=~D, size=~D"
                     column-name column-type column-size)
        
        (when (= column-type oci:+sqlt-num+)
          ;; the type of the precision attribute is 'oci:sb-2, because we
          ;; use an implicit describe here (would be sb-1 for explicit describe)
          (setf precision (oci-attr-get oci:+attr-precision+ 'oci:sb-2)
                scale (oci-attr-get oci:+attr-scale+ 'oci:sb-1)))

        (setf typemap (typemap-for-internal-type column-type
                                                 column-size
                                                 :precision precision
                                                 :scale scale))

        (multiple-value-bind (buffer size)
            (allocate-buffer-for-column typemap column-size +number-of-buffered-rows+)
          (oci:define-by-pos
              (statement-handle-of statement)
              define-handle-pointer
            (error-handle-of transaction)
            position
            buffer
            size
            (typemap-external-type typemap)
            indicators
            null
            return-codes
            *default-oci-flags*)

          (make-instance 'column-descriptor
                         :define-handle-pointer define-handle-pointer
                         :name column-name
                         :size size
                         :buffer buffer
                         :typemap typemap
                         :return-codes return-codes
                         :indicators indicators))))))

(defun allocate-buffer-for-column (typemap column-size number-of-rows)
  "Returns buffer, buffer-size"
  (let ((size (data-size-for (typemap-external-type typemap) column-size)))
    (values
     (cffi:foreign-alloc :uint8 :count (* size number-of-rows) :initial-element 0)
     size)))


(defun refill-result-buffers (cursor)
  (log.debug "Fill SQL result buffer")
  (let* ((transaction (transaction-of cursor))
         (statement (statement-of cursor))
         (statement-handle (statement-handle-of statement)))
    (setf (current-row-index-of cursor) 0
          (buffered-row-count-of cursor) 0)
    (unless (end-seen-p cursor)
      (let ((oci-code (oci:stmt-fetch
                       statement-handle
                       (error-handle-of transaction)
                       +number-of-buffered-rows+
                       oci:+fetch-next+
                       *default-oci-flags*)))
        (ecase oci-code
          (#.oci:+success+)
          (#.oci:+no-data+ (setf (end-seen-p cursor) #t))
          (#.oci:+error+ (handle-oci-error))))
      (let ((row-count (get-statement-attribute statement oci:+attr-row-count+ 'oci:ub-4)))
        (setf (buffered-row-count-of cursor)
              (- row-count (cumulative-row-count-of cursor)))
        (when (< (buffered-row-count-of cursor) +number-of-buffered-rows+)
          (setf (end-seen-p cursor) #t))
        (setf (cumulative-row-count-of cursor)
              row-count)))
    (values)))

(defun fetch-row (cursor &optional (eof-errorp nil) eof-value)
  #+nil(log.debug "Fetching row, buffered rows: ~A, current row: ~A"
                  (buffered-row-count-of cursor) (current-row-index-of cursor))
  (cond ((< (current-row-index-of cursor)
            (buffered-row-count-of cursor))
         (aprog1
             (loop with row-index = (current-row-index-of cursor)
                   for descriptor across (column-descriptors-of cursor)
                   collect (fetch-column-value descriptor row-index))
           (incf (current-row-index-of cursor))))

        ((not (end-seen-p cursor))
         (refill-result-buffers cursor)
         (fetch-row cursor eof-errorp eof-value))

        (eof-errorp
         (error 'simple-rdbms-error
                :format-control "no more rows available in ~S"
                :format-arguments (list cursor)))
        
        (t
         (return-from fetch-row eof-value))))

(defun fetch-column-value (column-descriptor row-index)
  (let* ((indicator (cffi:mem-aref (indicators-of column-descriptor) :short row-index)))
    (if (= indicator -1)
        :null
        (let* ((buffer (buffer-of column-descriptor))
               (size (size-of column-descriptor))
               (converter (typemap-oci-to-lisp (typemap-of column-descriptor)))
               result)
          #+nil
          (log.dribble "Buffer:~%~A"
                       (with-output-to-string (s)
                                              (loop for i from 0 below (* size +number-of-buffered-rows+)
                               do (format s "~2,'0X "
                                          (cffi:mem-ref buffer :uint8 i)))))
          (log.dribble "Convert from ~D, size is ~D, content:~%~A"
                       (typemap-external-type (typemap-of column-descriptor)) size
                       (with-output-to-string (s)
                         (loop for i from 0 below size
                               do (format s "~2,'0X "
                                          (cffi:mem-ref buffer :uint8 (+ (* row-index size) i))))))

          (setf result (funcall converter
                                (cffi:inc-pointer buffer (* row-index size))
                                size))

          (log.dribble "Converted to: ~S" result)

          result))))

(defun free-cursor (cursor)
  (loop for descriptor across (column-descriptors-of cursor)
        do (progn (cffi:foreign-free (buffer-of descriptor))
                  (cffi:foreign-free (indicators-of descriptor))
                  (cffi:foreign-free (return-codes-of descriptor)))))




