;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

;;;; TODO: finish sqlite backend, free allocated handles
;;;; TODO: implement proper error handling
(in-package :hu.dwim.rdbms.sqlite)

(def (class* e) sqlite (database)
  ())

(def method initialize-instance :before ((self sqlite) &key &allow-other-keys)
  (cffi:load-foreign-library 'hu.dwim.rdbms.sqlite::sqlite3))

(def class* sqlite-transaction (transaction)
  ((connection-pointer nil)))

(def method transaction-mixin-class list ((db sqlite))
  'sqlite-transaction)

(def class* sqlite-prepared-statement (prepared-statement)
  ((statement-pointer nil)))

(def constant +maximum-rdbms-name-length+ 30)

;; this name mapping is not injective, different lisp names _may_ be mapped to the same rdbms name
(def method calculate-rdbms-name ((db sqlite) thing name)
  (calculate-rdbms-name-with-utf-8-length-limit name +maximum-rdbms-name-length+ :prefix "_"))

(def function process-error (tr message &rest args)
  (apply 'process-error-code (sqlite3-cffi-bindings:sqlite-3-errcode (connection-pointer-of tr))
         (sqlite3-cffi-bindings:sqlite-3-errmsg (connection-pointer-of tr))
         message args))

(def function process-error-code (error-code error-message message &rest args)
  (unless (= sqlite3-cffi-bindings:+sqlite-ok+ error-code)
    (apply 'error (concatenate-string message "~%Error Code: ~A, Error Message: ~A")
           (append args (list error-code error-message)))))

(def function ensure-connected (tr)
  (unless (connection-pointer-of tr)
    (bind ((connection-pointer (cffi:foreign-alloc :pointer)))
      (sqlite3-cffi-bindings:sqlite-3-open (getf (connection-specification-of (database-of tr)) :file-name) connection-pointer)
      (setf (connection-pointer-of tr) (cffi:mem-ref connection-pointer :pointer))
      (cffi:foreign-free connection-pointer)
      (process-error tr "Error during opening database"))))

;; TODO: when will this prepared statement freed?
(def method prepare-command ((db sqlite) (tr sqlite-transaction) (command string) &key &allow-other-keys)
  (ensure-connected tr)
  (bind ((foreign-statement-pointer (cffi:foreign-alloc :pointer)))
    (cffi:with-foreign-string (foreign-command command)
      (sqlite3-cffi-bindings:sqlite-3-prepare-v-2 (connection-pointer-of tr) foreign-command -1 foreign-statement-pointer (cffi:null-pointer)))
    (make-instance 'sqlite-prepared-statement :statement-pointer (cffi:mem-ref foreign-statement-pointer :pointer))))

(def method execute-command ((db sqlite) (tr sqlite-transaction) (command string) &key binding-types binding-values result-type &allow-other-keys)
  (ensure-connected tr)
  (cffi:with-foreign-string (foreign-command command)
    (cffi:with-foreign-object (foreign-statement-pointer :pointer)
      (sqlite3-cffi-bindings:sqlite-3-prepare-v-2 (connection-pointer-of tr) foreign-command -1 foreign-statement-pointer (cffi:null-pointer))
      (bind ((foreign-statement (cffi:mem-ref foreign-statement-pointer :pointer)))
        (unwind-protect
             (execute-prepared-statment foreign-statement binding-types binding-values result-type)
          (sqlite3-cffi-bindings:sqlite-3-finalize foreign-statement))))))

(def method execute-command ((db sqlite) (tr sqlite-transaction) (prepared-statement sqlite-prepared-statement) &key binding-types binding-values result-type &allow-other-keys)
  (bind ((foreign-statement (statement-pointer-of prepared-statement)))
    (sqlite3-cffi-bindings:sqlite-3-reset foreign-statement)
    (sqlite3-cffi-bindings:sqlite-3-clear-bindings foreign-statement) 
    (execute-prepared-statment foreign-statement binding-types binding-values result-type)))

(def method cleanup-transaction :after ((tr sqlite-transaction))
  (awhen (connection-pointer-of tr)
    (process-error-code (sqlite3-cffi-bindings:sqlite-3-close it) nil "Error during closing database")))

(def function execute-prepared-statment (foreign-statement binding-types binding-values result-type)
  (prog1-bind result
      (ecase result-type
        (vector (make-array 8 :adjustable t :fill-pointer 0))
        (list nil))
    (iter (with sqlite3-transient = (cffi:inc-pointer (cffi:null-pointer) -1))
          (for i :from 1)
          (for binding-type :in-vector binding-types)
          (for binding-value :in-vector binding-values)
          (flet ((bind-string (value)
                   (sqlite3-cffi-bindings:sqlite-3-bind-text foreign-statement i (cffi:convert-to-foreign value :string) -1 sqlite3-transient))
                 (bind-array (value)
                   (cffi:with-foreign-pointer (blob (length value))
                     (iter (for i :from 0)
                           (for el :in-vector value)
                           (setf (cffi:mem-ref blob :int8 i) el))
                     (sqlite3-cffi-bindings:sqlite-3-bind-blob foreign-statement i blob (length value) sqlite3-transient))))
            (process-error-code
             (cond ((typep binding-type 'sql-boolean-type)
                    (sqlite3-cffi-bindings:sqlite-3-bind-int foreign-statement i (if binding-value 1 0)))
                   ((typep binding-type 'sql-integer-type)
                    (if (typep binding-value '(signed-byte 64))
                        (sqlite3-cffi-bindings:sqlite-3-bind-int-64 foreign-statement i binding-value)
                        ;; TODO: bind as a blob
                        (error "Integer ~A does not fit into (signed-byte 64)" binding-value)))
                   ((typep binding-type 'sql-float-type)
                    (sqlite3-cffi-bindings:sqlite-3-bind-double foreign-statement i (coerce binding-value 'double-float)))
                   ((typep binding-type 'sql-string-type)
                    (bind-string binding-value))
                   ((typep binding-type 'sql-date-type)
                    (bind-string (format-rfc3339-timestring nil binding-value :omit-time-part #t :omit-timezone-part #t)))
                   ((typep binding-type 'sql-time-type)
                    (bind-string (format-rfc3339-timestring nil binding-value :omit-date-part #t :omit-timezone-part #t)))
                   ((typep binding-type 'sql-timestamp-type)
                    (bind-string (format-rfc3339-timestring nil binding-value)))
                   ((typep binding-type 'sql-binary-large-object-type)
                    (bind-array binding-value))
                   (t (error "Unknown type ~A" binding-type)))
             nil "Cannot bind parameter $~A to ~A" i binding-value)))
    (iter (with column-count = (sqlite3-cffi-bindings:sqlite-3-column-count foreign-statement))
          (for step = (sqlite3-cffi-bindings:sqlite-3-step foreign-statement))
          (until (eq step sqlite3-cffi-bindings:+sqlite-done+))
          (unless (eq step sqlite3-cffi-bindings:+sqlite-row+)
            (error "Error during stepping: ~A" step))
          (for row = (ecase result-type
                       (vector (make-array column-count))
                       (list nil)))
          (iter (for i :from 0 :below column-count)
                (for type = (sqlite3-cffi-bindings:sqlite-3-column-type foreign-statement i))
                (for value = (ecase type
                               ;; TODO: how do we recognize boolean stored as integer,
                               ;; TODO: how do we recognize date, time, timestamp stored as string
                               ;; TODO: how do we recognize big integers stored as blobs?
                               (#.sqlite3-cffi-bindings:+sqlite-null+ :null)
                               (#.sqlite3-cffi-bindings:+sqlite-integer+ (sqlite3-cffi-bindings:sqlite-3-column-int-64 foreign-statement i))
                               (#.sqlite3-cffi-bindings:+sqlite-float+ (sqlite3-cffi-bindings:sqlite-3-column-double foreign-statement i))
                               (#.sqlite3-cffi-bindings:+sqlite-text+ (cffi:convert-from-foreign (sqlite3-cffi-bindings:sqlite-3-column-text foreign-statement i) :string))
                               (#.sqlite3-cffi-bindings:+sqlite-blob+
                                (iter (with size = (sqlite3-cffi-bindings:sqlite-3-column-bytes foreign-statement i))
                                      (with blob = (make-array size))
                                      (with pointer = (sqlite3-cffi-bindings:sqlite-3-column-blob foreign-statement i))
                                      (for i :from 0 :below size)
                                      (setf (aref blob i) (cffi:mem-ref pointer :int8 i))
                                      (finally (return blob))))))
                (ecase result-type
                  (vector (setf (aref row i) value))
                  (list (push value row))))
          (ecase result-type
            (vector (vector-push-extend row result))
            (list (push (nreverse row) result))))
    (when (eq result-type 'list)
      (setf result (nreverse result)))))
