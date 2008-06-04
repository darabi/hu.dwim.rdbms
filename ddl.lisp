;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Create, drop and alter table

(def (function e) create-table (name columns &key temporary)
  (execute-ddl (make-instance 'sql-create-table :temporary temporary :name name :columns columns)))

(def (function e) create-temporary-table (name &rest columns)
  (execute-ddl (make-instance 'sql-create-table :name name :columns columns)))

(def (function e) drop-table (name &key cascade)
  (execute-ddl (make-instance 'sql-drop-table :name name :cascade cascade)))

(def (function e) alter-table (name &rest actions)
  (execute-ddl (make-instance 'sql-alter-table :name name :actions actions)))

(def (function e) add-column (name column)
  (execute-ddl (make-instance 'sql-alter-table
                              :name name
                              :actions (list (make-instance 'sql-add-column-action
                                                            :name (name-of column)
                                                            :type (type-of column))))))

(def (function e) drop-column (name column-name &optional (cascade #f))
  (execute-ddl (make-instance 'sql-alter-table
                              :name name
                              :actions (list (make-instance 'sql-drop-column-action :name column-name :cascade cascade)))))

(def (function e) alter-column-type (name column)
  (execute-ddl (make-instance 'sql-alter-table
                              :name name
                              :actions (list (make-instance 'sql-alter-column-type-action
                                                            :name (name-of column)
                                                            :type (type-of column))))))

(def (function e) add-primary-key-constraint (name columns)
  (execute-ddl (make-instance 'sql-alter-table
                              :name name
                              :actions (list (make-instance 'sql-add-primary-key-constraint-action :columns columns)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Query tables and columns

(def (function e) list-tables ()
  (database-list-tables *database*))

(defgeneric database-list-tables (database)
  (:documentation "Returns the list of table names present in the database."))

(def (function e) list-table-columns (name)
  (database-list-table-columns name *database*))

(defgeneric database-list-table-columns (name database)
  (:documentation "Returns the list of columns present in the database."))

(def (function e) table-exists-p (name)
  (not (null (member (string-downcase name) (list-tables) :test 'equalp))))

;;;;;;;;;;;;;;;;
;;; Update table

(defparameter *signal-non-destructive-alter-table-commands* #f)

(def (condition* e) unconfirmed-alter-table-error (serious-condition)
  ((table-name
    :type string)
   (column-name
    :type string)))

(def (condition* e) unconfirmed-add-column-error (unconfirmed-alter-table-error)
  ((column-type))
  (:report (lambda (error stream)
             (format stream "Adding the column ~S with type ~A in table ~S is a safe operation"
                     (column-name-of error) (column-type-of error) (table-name-of error)))))

(def (condition* e) unconfirmed-alter-column-type-error (unconfirmed-alter-table-error)
  ((old-type)
   (new-type)
   (new-rdbms-type))
  (:report (lambda (error stream)
             (format stream "Changing the type of column ~S from ~A to ~A, ~A in table ~S will be issued in a separate transaction and your database will try to convert existing data which may be an unsafe operation"
                     (column-name-of error) (old-type-of error) (new-type-of error)
                     (new-rdbms-type-of error) (table-name-of error)))))

(def (condition* e) unconfirmed-destructive-alter-table-error (unconfirmed-alter-table-error)
  ())

(def (condition* e) unconfirmed-destructive-alter-column-type-error (unconfirmed-destructive-alter-table-error
                                                                     unconfirmed-alter-column-type-error)
  ()
  (:report (lambda (error stream)
             (format stream "DESTRUCTIVE: Changing the type of column ~S from ~A to ~A, ~A in table ~S is a destructive transformation"
                     (column-name-of error) (old-type-of error) (new-type-of error)
                     (new-rdbms-type-of error) (table-name-of error)))))

(def (condition* e) unconfirmed-destructive-drop-column-error (unconfirmed-destructive-alter-table-error)
  ()
  (:report (lambda (error stream)
             (format stream "DESTRUCTIVE: Dropping the column ~S from table ~S is a destructive transformation"
                     (column-name-of error) (table-name-of error)))))

(def (macro e) with-confirmed-descructive-changes (&body body)
  `(handler-bind ((unconfirmed-destructive-alter-table-error #'continue))
    ,@body))

(def (function e) update-table (name columns)
  (if (table-exists-p name)
      (update-existing-table name columns)
      (create-table name columns)))

(defgeneric rdbms-type-for (type database)
  (:documentation "Maps the given type to the smallest matching type.")

  (:method (type database)
           type))

(defgeneric equal-type-p (type-1 type-2 database)
  (:method (type-1 type-2 database)
           #f))

(defun update-existing-table (name columns)
  (let ((table-columns (list-table-columns name)))
    ;; create new columns that are missing from the table
    (dolist (column columns)
      (let* ((column-name (name-of column))
	     (table-column (find (string-downcase column-name) table-columns :key #'name-of :test #'equalp)))
	(if table-column
            ;; change column type where needed
            (let ((new-type (rdbms-type-for (type-of column) *database*)))
              (unless (equal-type-p (type-of table-column) new-type *database*)
                (handler-case
                    (with-transaction
                      (with-simple-restart
                          (continue "Alter the table by adding the new column")
                        (when *signal-non-destructive-alter-table-commands*
                          (error 'unconfirmed-alter-column-type-error :table-name name :column-name column-name
                                 :old-type (type-of table-column)
                                 :new-type (type-of column)
                                 :new-rdbms-type new-type)))
                      (alter-column-type name column))
                  (error (e)
                    (declare (ignore e))
                    (with-simple-restart
                        (continue "Alter the table and let the data go")
                      (error 'unconfirmed-destructive-alter-column-type-error :table-name name :column-name column-name
                             :old-type (type-of table-column)
                             :new-type (type-of column)
                             :new-rdbms-type new-type))
                    (drop-column name column-name #t)
                    (add-column name column)))))
            ;; add missing columns not present in the table
            (progn
              (with-simple-restart
                  (continue "Alter the table by adding the new column")
                (when *signal-non-destructive-alter-table-commands*
                  (error 'unconfirmed-add-column-error :table-name name :column-name (name-of column) :column-type (type-of column))))
              (add-column name column)))))
    ;; drop extra columns that are present in the table
    (dolist (table-column table-columns)
      (let ((column-name (name-of table-column)))
	(unless (find column-name columns :key (compose #'string-downcase #'name-of) :test #'equalp)
          (with-simple-restart
              (continue "Alter the table and let the data go")
            (error 'unconfirmed-destructive-drop-column-error :table-name name :column-name column-name))
	  (drop-column name column-name #t))))))

;;;;;;;;;;;;;;;;;;;;;
;;; Create, drop view

(def (function e) create-view (name columns as)
  (execute-ddl (make-instance 'sql-create-view
                              :name name
                              :columns columns
                              :as as)))

(def (function e) drop-view (name)
  (execute-ddl (make-instance 'sql-drop-view :name name)))

(def (function e) view-exists-p (name)
  (not (null (member (string-downcase name) (list-views) :test 'equalp))))

(def (function e) update-view (name column as)
  (when (view-exists-p name)
    (drop-view name))
  (create-view name column as))

(def (function e) list-views ()
  (database-list-views *database*))

(defgeneric database-list-views (database)
  (:documentation "Returns the list of view names present in the database."))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Create, drop sequence

(def (function e) create-sequence (name)
  (execute-ddl (make-instance 'sql-create-sequence :name name)))

(def (function e) drop-sequence (name)
  (execute-ddl (make-instance 'sql-drop-sequence :name name)))

(def (function e) sequence-exists-p (name)
  (not (null (member (string-downcase name) (list-sequences) :test 'equalp))))

(def (function e) list-sequences ()
  (database-list-sequences *database*))

(defgeneric database-list-sequences (database)
  (:documentation "Returns the list of sequence names present in the database."))

(def (function e) sequence-next (name)
  (first* (first* (execute (make-instance 'sql-select :columns (list (make-instance 'sql-sequence-nextval-column :name name)))))))

;;;;;;;;;;;;;;;;;;;;;;
;;; Create, drop index

(def (function e) (name table-name columns &key (unique #f))
  (execute-ddl (make-instance 'sql-create-index
                              :name name
                              :table-name table-name
                              :columns columns
                              :unique unique)))

(def (function e) drop-index (name)
  (execute-ddl (make-instance 'sql-drop-index :name name)))

(def (function e) update-index (name table-name columns &key (unique #f))
  ;; TODO: where clause for unique
  (unless (find name (list-table-indices table-name)
                :key 'name-of
                :test (lambda (o1 o2)
                        (equalp (string-downcase o1)
                                (string-downcase o2))))
    (create-index name table-name columns :unique unique)))

(def (function e) list-table-indices (name)
  (database-list-table-indices name *database*))

(defgeneric database-list-table-indices (name database)
  (:documentation "Returns the list of table indices present in the database."))
