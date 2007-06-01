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
                              oci:+default+)))

(defmethod rollback-transaction ((database oracle) (transaction oracle-transaction))
  (oci-call (oci:trans-rollback (service-context-handle-of transaction)
                                (error-handle-of transaction)
                                oci:+default+)))

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
(define-symbol-macro null (cffi:null-pointer))

(defmacro ignore-errors* (&body body)
  `(block nil
    (handler-bind ((serious-condition
                    (lambda (error)
                      (log.warn "Ignoring error: ~A" error)
                      (return))))
      ,@body)))

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
                     null
                     error-code
                     error-buffer
                     oci:+error-maxmsg-size+ oci:+htype-error+)

      (let ((error-message (cffi:foreign-string-to-lisp error-buffer)))
        (log.error "Signalling error: ~A" error-message)
        (simple-rdbms-error "RDBMS error: ~A" error-message)))))

(defun make-void-pointer ()
  (cffi:foreign-alloc '(:pointer :void)))

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
                              oci:+default+ null null null null 0 null))

    (oci-call (oci:handle-alloc (environment-handle-of transaction)
                                (error-handle-pointer transaction)
                                oci:+htype-error+ 0 null))

    (oci-call (oci:handle-alloc (environment-handle-of transaction)
                                (server-handle-pointer transaction)
                                oci:+htype-server+ 0 null))

    (oci-call (oci:handle-alloc (environment-handle-of transaction)
                                (service-context-handle-pointer transaction)
                                oci:+htype-svcctx+ 0 null))

    (log.debug "Logging on in transaction ~A" transaction)
    (cffi:with-foreign-string (c-user-name user-name) ;; TODO CFFI mapping should use :string
      (cffi::with-foreign-string (c-password password)
        (cffi:with-foreign-string (c-datasource datasource)
          (cffi:with-foreign-string (c-empty-string "")
            (oci-call (oci:server-attach (server-handle-of transaction)
                                         (error-handle-of transaction)
                                         c-datasource
                                         (length datasource) 0))

            (oci-call (oci:attr-set (service-context-handle-of transaction)
                                    oci:+htype-svcctx+
                                    (server-handle-of transaction)
                                    0
                                    oci:+attr-server+
                                    (error-handle-of transaction)))
            
            (oci-call (oci:handle-alloc (environment-handle-of transaction)
                                        (session-handle-pointer transaction)
                                        oci:+htype-session+ 0 null))

            (oci-call (oci:attr-set (session-handle-of transaction)
                                    oci:+htype-session+
                                    c-user-name
                                    (length user-name)
                                    oci:+attr-username+
                                    (error-handle-of transaction)))

            (oci-call (oci:attr-set (session-handle-of transaction)
                                    oci:+htype-session+
                                    c-password
                                    (length password)
                                    oci:+attr-password+
                                    (error-handle-of transaction)))

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
                                    (error-handle-of transaction)))))))))

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
    (oci-call (oci:handle-alloc (environment-handle-of transaction)
                                (statement-handle-pointer statement)
                                oci:+htype-stmt+ 0 null))
    (cffi:with-foreign-string (c-command command)
      (oci-call (oci:stmt-prepare (statement-handle-of statement)
                                  (error-handle-of transaction)
                                  c-command
                                  (length command)
                                  oci:+ntv-syntax+ oci:+default+)))
    (log.dribble "Statement is allocated")
    (setf (select-p statement) (= (get-statement-attribute
                                   statement
                                   oci:+attr-stmt-type+
                                   'oci:ub-2)
                                  oci:+stmt-select+))
    statement))

(defun free-prepared-statement (statement)
  (oci-call (oci:handle-free (statement-handle-of statement) oci:+htype-stmt+))
  (cffi:foreign-free (statement-handle-pointer statement)))

(defun execute-prepared-statement (transaction statement bindings visitor)
  (declare (ignore bindings))
  (let ((iters (if (select-p statement) 0 1)))

    ;; TODO binding

    (oci-call (oci:stmt-execute (service-context-handle-of transaction)
                                (statement-handle-of statement)
                                (error-handle-of transaction)
                                iters 0 null null oci:+default+)))
  
  #+nil
  (let ((column-count nil))
    (setf column-count (get-statement-attribute
                        statement
                        oci:+attr-param-count+
                        'oci:ub-2))
    (log.dribble "Resultset has ~S columns" column-count)
    (loop for column-idx :below column-count
          do (progn
               (get-column-value transaction statement column-idx))))
  
  (cond
    ((select-p statement)
     (let ((cursor (make-cursor statement transaction)))
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

#+nil
(defun get-column-value (transaction statement column-idx)
  (let ((param-descriptor nil)
        (column-name nil)
        (column-type nil))
    (cffi:with-foreign-object (param-descriptor-pointer :pointer)
      (oci-call (oci:param-get (statement-handle-of statement)
                               oci:+htype-stmt+
                               (error-handle-of transaction)
                               param-descriptor-pointer
                               (1+ column-idx)))
      (setf param-descriptor (cffi:mem-ref param-descriptor-pointer :pointer)))

    (setf column-type (get-param-descriptor-attribute
                       param-descriptor
                       oci:+attr-data-type+
                       'oci:ub-2))

    (setf column-name (get-param-descriptor-attribute
                       param-descriptor
                       oci:+attr-name+ :string))
                   
    (log.dribble "Column type of ~S is ~A" column-name column-type)))


(defconstant +number-of-buffered-rows+ 200)

(defclass* cursor ()
  ((statement)
   (transaction)
   (column-descriptors)
   (cumulative-row-count 0)
   (buffered-row-count 0)
   (current-row-index 0)
   (end-seen #f :type boolean)))

(defclass* column-descriptor ()
  ((name)
   (size)
   (buffer)
   (oci-data-type)
   (indicators)
   (return-codes)))

(defun make-cursor (statement transaction)
  (let ((cursor (make-instance 'cursor :statement statement :transaction transaction)))
    (setf (column-descriptors-of cursor)
          (make-column-descriptors statement transaction))
    cursor))

(defun make-column-descriptors (statement transaction)
  (let ((statement-handle (statement-handle-of statement))
        (error-handle (error-handle-of transaction)))
    (cffi:with-foreign-objects ((parmdp '(:pointer :void))
                                (dtype-foreign  :unsigned-short)
                                (precision :short)
                                (scale :int8) ; :byte
                                (colname '(:pointer :unsigned-char))
                                (colnamelen 'oci:ub-4)
                                (colsize 'oci:ub-2)
                                (defnp '(:pointer :void)))
      (do ((column-index 0 (1+ column-index))
           (descriptors nil))
          ((not (eql (oci:param-get statement-handle
                                    oci:+htype-stmt+
                                    error-handle
                                    parmdp
                                    (1+ column-index))
                     oci:+success+))
           (coerce (reverse descriptors) 'simple-vector))

        (labels ((%oci-attr-get (attribute-id attribute-pointer &optional (size-pointer null))
                   (oci-call (oci:attr-get (cffi:mem-ref parmdp '(:pointer :void))
                                           oci:+dtype-param+
                                           attribute-pointer
                                           size-pointer
                                           attribute-id
                                           error-handle)))
                 (oci-attr-get (attribute-id cffi-type attribute-pointer)
                   (%oci-attr-get attribute-id attribute-pointer)
                   (cffi:mem-ref attribute-pointer cffi-type))
                 (oci-string-attr-get (attribute-id attribute-pointer size-pointer)
                   (%oci-attr-get attribute-id attribute-pointer size-pointer)
                   (cffi:foreign-string-to-lisp
                    (cffi:mem-ref attribute-pointer '(:pointer :unsigned-char))
                    (cffi:mem-ref size-pointer 'oci:ub-4))))
              
            
          (let ((column-name (oci-string-attr-get oci:+attr-name+ colname colnamelen))
                (dtype (oci-attr-get oci:+attr-data-type+ :unsigned-short dtype-foreign))
                (size nil)
                (buffer nil)
                (return-codes (cffi:foreign-alloc :unsigned-short :count +number-of-buffered-rows+))
                (indicators (cffi:foreign-alloc :short :count +number-of-buffered-rows+)))
            (declare (fixnum dtype))
            (case dtype
              (#.oci:+sqlt-dat+
               (setf buffer (cffi:foreign-alloc :unsigned-char
                                                :count (* 32 +number-of-buffered-rows+))
                     size 32
                     dtype #.oci:+sqlt-dat+))
              (#.oci:+sqlt-num+
               (let ((*scale (oci-attr-get oci:+attr-scale+ :int8 scale))
                     (*precision (oci-attr-get oci:+attr-precision+ :short precision)))
                 (if (or (and (minusp *scale) (zerop *precision))
                         (and (zerop *scale) (plusp *precision)))
                     (setf buffer (cffi:foreign-alloc :int :count +number-of-buffered-rows+)
                           size 4 ;; sizeof(int)
                           dtype #.oci:+sqlt-int+)
                     (setf buffer (cffi:foreign-alloc :double :count +number-of-buffered-rows+)
                           size 8 ;; sizeof(double)
                           dtype #.oci:+sqlt-flt+))))
              ;; Default to SQLT-STR
              (t
               (setf (cffi:mem-ref colsize :unsigned-short) 0)
               (let ((colsize-including-null
                      (1+ (oci-attr-get oci:+attr-data-size+ :unsigned-short colsize))))
                 (setf buffer (cffi:foreign-alloc
                               :unsigned-char :count (* +number-of-buffered-rows+
                                                        colsize-including-null))
                       size colsize-including-null
                       dtype #.oci:+sqlt-str+))))

            (push (make-instance 'column-descriptor
                                 :name column-name
                                 :size size
                                 :buffer buffer
                                 :oci-data-type dtype
                                 :return-codes return-codes
                                 :indicators indicators)
                  descriptors)
            (oci:define-by-pos
                statement-handle
                defnp
              error-handle
              (1+ column-index)         ; OCI 1-based indexing again
              buffer
              size
              dtype
              indicators
              null
              return-codes
              oci:+default+)))))))


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
                       oci:+fetch-next+ oci:+default+)))
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

  (when (>= (current-row-index-of cursor)
            (buffered-row-count-of cursor))
    (if (end-seen-p cursor)
        (if eof-errorp
            (error 'simple-rdbms-error
                   :format-control "no more rows available in ~S"
                   :format-arguments (list cursor))
            (return-from fetch-row eof-value))
        (refill-result-buffers cursor)))

  (aprog1
      (loop with row-index = (current-row-index-of cursor)
            for descriptor across (column-descriptors-of cursor)
            collect (fetch-column-value descriptor row-index))
    (incf (current-row-index-of cursor))))

(defun fetch-column-value (column-descriptor row-index)
  (let* ((buffer (buffer-of column-descriptor))
         (indicator (cffi:mem-aref (indicators-of column-descriptor) :short row-index)))
    (if (= indicator -1)
        :null
        (ecase (oci-data-type-of column-descriptor)
          ((#.oci:+sqlt-str+ #.oci:+sqlt-dat+)
           (cffi:foreign-string-to-lisp
            (cffi:make-pointer
             (+ (cffi:pointer-address buffer) (* row-index (size-of column-descriptor))))))
          (#.oci:+sqlt-flt+
           (cffi:mem-aref buffer :double row-index))
          (#.oci:+sqlt-int+
           (cffi:mem-aref buffer :int row-index))))))

(defun free-cursor (cursor)
  (loop for descriptor across (column-descriptors-of cursor)
        do (progn (cffi:foreign-free (buffer-of descriptor))
                  (cffi:foreign-free (indicators-of descriptor))
                  (cffi:foreign-free (return-codes-of descriptor)))))

