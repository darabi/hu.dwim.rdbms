;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms.oracle)

#.(file-header)

(publish-backend-symbol 'oracle)

;;;----------------------------------------------------------------------------
;;; Backend API
;;;

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
  (make-prepared-statement command))

(defmethod execute-command ((database oracle)
                            (transaction oracle-transaction)
                            (command string)
                            &key visitor bindings result-type start-row row-limit
                            &allow-other-keys)
  (log.debug "Executing ~S" command)
  (let ((statement (prepare-command database transaction command)))
    (unwind-protect
         (execute-prepared-statement transaction statement bindings visitor result-type
                                     :start-row start-row :row-limit row-limit)
      (free-prepared-statement statement))))

(defmethod execute-command ((database oracle)
                            (transaction oracle-transaction)
                            (prepared-statement prepared-statement)
                            &key visitor bindings result-type start-row row-limit
                            &allow-other-keys)
  (execute-prepared-statement transaction prepared-statement bindings visitor result-type
                              :start-row start-row :row-limit row-limit))

(defmethod cleanup-transaction ((transaction oracle-transaction))
  (when (environment-handle-pointer transaction)
    (log.debug "Cleaning up Oracle transaction ~A to database ~A" transaction (database-of transaction))
    (disconnect transaction)))

;;;----------------------------------------------------------------------------
;;; Connection
;;;

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

    (iter connecting
          (with-simple-restart (retry "Retry connecting to Oracle")
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
                                    (error-handle-of transaction)))
            (return-from connecting))
          (unless (cffi:null-pointer-p (session-handle-of transaction))
            (oci-call (oci:handle-free (session-handle-of transaction) oci:+htype-session+))
            (setf (session-handle-of transaction) null)))))

(defmacro ignore-errors* (&body body)
  `(block nil
    (handler-bind ((serious-condition
                    (lambda (error)
                      (log.warn "Ignoring error: ~A" error)
                      (return))))
      ,@body)))

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

;;;----------------------------------------------------------------------------
;;; Prepared statement
;;;

(defun make-prepared-statement (command &optional (name ""))
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

(defun execute-prepared-statement (transaction statement bindings visitor result-type
                                               &key (start-row 0) row-limit)

  (let ((needs-scrollable-cursor-p (and start-row (> start-row 0))))
    ;; make bindings
    (setf (bindings-of statement) (make-bindings statement transaction bindings))
    
    ;; execute
    (stmt-execute statement
                  (if needs-scrollable-cursor-p
                      (logior *default-oci-flags* oci:+stmt-scrollable-readonly+)
                      *default-oci-flags*))
  
    ;; fetch
    (cond
      ((select-p statement)
       (let* ((cursor (make-cursor transaction
                                   :statement statement
                                   :result-type result-type
                                   :random-access-p needs-scrollable-cursor-p)))
         (unwind-protect
              (if visitor
                  (for-each-row visitor cursor
                                :start-position start-row
                                :row-count row-limit)
                  (collect-rows cursor
                                :start-position start-row
                                :row-count row-limit))
           (close-cursor cursor))))
      (t
       nil))))

;;;----------------------------------------------------------------------------
;;; Binding
;;;
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

;;;----------------------------------------------------------------------------
;;; Cursor
;;;
(defconstant +number-of-buffered-rows+ 1) ; TODO prefetching rows probably superfluous,
                                          ; because OCI does that

(defclass* column-descriptor ()
  ((define-handle-pointer)
   (name)
   (size)
   (buffer)
   (typemap)
   (indicators)
   (return-codes)))

(defun allocate-buffer-for-column (typemap column-size number-of-rows)
  "Returns buffer, buffer-size"
  (let* ((external-type (typemap-external-type typemap))
         (size (data-size-for external-type column-size))
         (ptr (cffi:foreign-alloc :uint8 :count (* size number-of-rows) :initial-element 0))
         (constructor (typemap-allocate-instance typemap)))

    (when constructor
      (loop for i from 0 below number-of-rows
            do (funcall constructor (cffi:inc-pointer ptr (* i size)))))
    
    (values
     ptr
     size)))

(defun free-column-descriptor (descriptor)
  (with-slots (size typemap buffer indicators return-codes) descriptor
    (let ((destructor (typemap-free-instance typemap)))
      (when destructor
        (loop for i from 0 below +number-of-buffered-rows+
              do (funcall destructor (cffi:mem-ref buffer :pointer (* i size))))))
    (cffi:foreign-free buffer)
    (cffi:foreign-free indicators)
    (cffi:foreign-free return-codes)))

(defclass* oracle-cursor (cursor)
  ((statement)
   (column-descriptors)
   (current-position 0)
   (buffer-start-position 0)
   (buffer-end-position 0)))

(defgeneric column-descriptor-of (cursor index)
  (:method ((cursor oracle-cursor) index)
           (svref (column-descriptors-of cursor) index)))

(defclass* oracle-sequential-access-cursor (oracle-cursor sequential-access-cursor)
  ((end-seen #f :type boolean)))

(defclass* oracle-random-access-cursor (oracle-cursor random-access-cursor)
  ((row-count nil)))


;;
;; Cursor API
;;
(defmethod make-cursor ((transaction oracle-transaction)
                        &key statement random-access-p &allow-other-keys)
  (if random-access-p
      (aprog1 (make-instance 'oracle-random-access-cursor
                             :statement statement
                             :column-descriptors (make-column-descriptors statement transaction))
        (row-count it)  ; TODO This positions to the last row so the response
                            ;      time can be high. Try to delay this computation
        (log.dribble "Count of rows: ~D" (row-count-of it)))
      (make-instance 'oracle-sequential-access-cursor
                     :statement statement
                     :column-descriptors (make-column-descriptors statement transaction))))

(defmethod close-cursor ((cursor oracle-cursor))
  (loop for descriptor across (column-descriptors-of cursor)
        do (free-column-descriptor descriptor)))

(defmethod cursor-position ((cursor oracle-sequential-access-cursor))
  (unless (end-seen-p cursor)
    (current-position-of cursor)))

(defmethod cursor-position ((cursor oracle-random-access-cursor))
  (with-slots (current-position row-count) cursor
    (when (and (<= 0 current-position) (< current-position row-count))
      current-position)))

(defmethod (setf cursor-position) (where (cursor oracle-sequential-access-cursor))
  (ecase where
    (:first (setf (current-position-of cursor) 0))
    (:next (incf (current-position-of cursor))))
  (ensure-current-position-is-buffered cursor)) ; TODO delay this call

(defmethod (setf cursor-position) (where (cursor oracle-random-access-cursor))
  (if (integerp where)
      (incf (current-position-of cursor) where)
      (ecase where
        (:first (setf (current-position-of cursor) 0))
        (:last (setf (current-position-of cursor) (1- (row-count cursor))))
        (:previous (decf (current-position-of cursor)))
        (:next (incf (current-position-of cursor))))))

(defmethod absolute-cursor-position ((cursor oracle-cursor))
  (current-position-of cursor)) ; FIXME always the same as (cursor-position cursor) ?

(defmethod (setf absolute-cursor-position) (where (cursor oracle-random-access-cursor))
  (setf (current-position-of cursor) where))

(defmethod row-count ((cursor oracle-sequential-access-cursor))
  (error "Row count not supported by ~S" cursor))

(defmethod row-count ((cursor oracle-random-access-cursor))
  (with-slots (row-count statement buffer-start-position buffer-end-position) cursor
    (unless row-count
      (if (stmt-fetch-last statement)
          (setf row-count (get-row-count-attribute statement)
                buffer-start-position (1- row-count)
                buffer-end-position row-count)
          (setf row-count 0)))
    row-count))

(defmethod column-count ((cursor oracle-cursor))
  (length (column-descriptors-of cursor)))

(defmethod column-name ((cursor oracle-cursor) index)
  (name-of (column-descriptor-of cursor index)))

(defmethod column-type ((cursor oracle-cursor) index)
  nil) ; TODO

(defmethod column-value ((cursor oracle-cursor) index)
  (when (ensure-current-position-is-buffered cursor)
    (fetch-column-value (column-descriptor-of cursor index)
                        (- (current-position-of cursor)
                           (buffer-start-position-of cursor)))))

#|
(defun current-row (cursor &key result-type)
  (when (ensure-current-position-is-buffered cursor)
    (with-slots (column-descriptors current-position buffer-start-position default-result-type) cursor
      (let ((row-index (- current-position buffer-start-position)))
        (ecase (or result-type default-result-type)
          (vector (aprog1 (make-array (length column-descriptors))
                    (loop for descriptor across column-descriptors
                          for column-index from 0
                          do (setf (svref it column-index)
                                   (fetch-column-value descriptor row-index)))))
          (list (loop for descriptor across column-descriptors
                      collect (fetch-column-value descriptor row-index))))))))
|#

;;;
;;; Cursor helpers
;;;
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

(defun make-column-descriptor (statement transaction position param-descriptor)
  (cffi:with-foreign-objects ((attribute-value :uint8 8) ; 8 byte buffer for attribute values
                              (attribute-value-length 'oci:ub-4))
    (flet ((oci-attr-get (attribute-id cffi-type)
             (oci-attr-get param-descriptor attribute-id attribute-value attribute-value-length)
             (cffi:mem-ref attribute-value cffi-type))
           (oci-string-attr-get (attribute-id)
             (oci-attr-get param-descriptor attribute-id attribute-value attribute-value-length)
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

(defgeneric ensure-current-position-is-buffered (cursor)
  (:method ((cursor oracle-sequential-access-cursor))
           (with-slots (statement current-position
                                  buffer-start-position buffer-end-position end-seen) cursor
             (cond
               ((< current-position buffer-start-position)
                (if (zerop current-position)
                    nil                 ; TODO reexecute statement
                    (error "Trying to go backwards with a sequential cursor: ~S" cursor)))
               (t
                (loop until (or (> buffer-end-position current-position)
                                end-seen)
                  do (if (stmt-fetch-next statement +number-of-buffered-rows+)
                         (setf buffer-start-position buffer-end-position
                               buffer-end-position (get-row-count-attribute statement))
                         (setf end-seen #t))
                  finally (return-from ensure-current-position-is-buffered (not end-seen)))))))

  (:method ((cursor oracle-random-access-cursor))
           (with-slots (statement current-position buffer-start-position buffer-end-position
                                  row-count) cursor
             (cond
               ((or (< current-position 0)
                    (>= current-position row-count))
                #f)
               ((>= current-position buffer-end-position) ; forward
                (stmt-fetch-2 statement +number-of-buffered-rows+
                              oci:+fetch-absolute+ (1+ current-position)) ; OCI 1-based indexing
                (setf buffer-start-position current-position
                      buffer-end-position (min (+ current-position +number-of-buffered-rows+)
                                               row-count)))
               ((< current-position buffer-start-position) ; backward
                (stmt-fetch-2 statement +number-of-buffered-rows+ oci:+fetch-absolute+
                              (1+ (max (- (1+ current-position) +number-of-buffered-rows+) 0))) ; 1-based
                (setf buffer-start-position (max (- (1+ current-position) +number-of-buffered-rows+) 0)
                      buffer-end-position (1+ current-position)))
               (t
                #t)))))

(defun fetch-column-value (column-descriptor row-index)
  (log.debug "Fetching ~S from buffer at index ~D" (name-of column-descriptor) row-index)
  (aprog1 (let* ((indicator (cffi:mem-aref (indicators-of column-descriptor) :short row-index)))
            (if (= indicator -1)
                :null
                (let* ((buffer (buffer-of column-descriptor))
                       (size (size-of column-descriptor))
                       (converter (typemap-oci-to-lisp (typemap-of column-descriptor))))
                  #+nil
                  (log.dribble "Buffer:~%~A"
                               (dump-c-byte-array buffer (* size +number-of-buffered-rows+)))
                  (log.dribble "Convert from ~D, size is ~D, content:~%~A"
                               (typemap-external-type (typemap-of column-descriptor)) size
                               (dump-c-byte-array (cffi:inc-pointer buffer (* row-index size))
                                                  size))

                  (funcall converter
                           (cffi:inc-pointer buffer (* row-index size))
                           size))))
    (log.debug "Fetched: ~S" it)))






