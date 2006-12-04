;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms)

#.(file-header)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Create, drop and alter table

(defun create-table (name columns &key temporary)
  (execute-ddl (make-instance 'sql-create-table :temporary temporary :name name :columns columns)))

(defun create-temporary-table (name &rest columns)
  (execute-ddl (make-instance 'sql-create-table :name name :columns columns)))

(defun drop-table (name)
  (execute-ddl (make-instance 'sql-drop-table :name name)))

(defun alter-table (name &rest actions)
  (execute-ddl (make-instance 'sql-alter-table :name name :actions actions)))

(defun add-column (name column)
  (execute-ddl (make-instance 'sql-alter-table
                              :name name
                              :actions (list (make-instance 'sql-add-column-action
                                                            :name (name-of column)
                                                            :type (type-of column))))))

(defun drop-column (name column-name)
  (execute-ddl (make-instance 'sql-alter-table
                              :name name
                              :actions (list (make-instance 'sql-drop-column-action :name column-name)))))

(defun alter-column-type (name column)
  (execute-ddl (make-instance 'sql-alter-table
                              :name name
                              :actions (list (make-instance 'sql-alter-column-type-action
                                                            :name (name-of column)
                                                            :type (type-of column))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Query tables and columns

(defun list-tables ()
  (database-list-tables *database*))

(defgeneric database-list-tables (database)
  (:documentation "Returns the list of table names present in the database."))

(defun list-table-columns (name)
  (database-list-table-columns name *database*))

(defgeneric database-list-table-columns (name database)
  (:documentation "Returns the list of columns present in the database."))

(defun table-exists-p (name)
  (not (null (member (string-downcase name) (list-tables) :test 'equalp))))

;;;;;;;;;;;;;;;;
;;; Update table

(defcondition* unconfirmed-destructive-alter-table-error (error)
  ((table-name
    :type string)
   (column-name
    :type string)))

(defcondition* unconfirmed-destructive-alter-column-type-error (unconfirmed-destructive-alter-table-error)
  ((new-type))
  (:report (lambda (error stream)
             (format stream "Changing the type of column ~S to ~S in table ~S is a destructive transformation"
                     (column-name-of error) (new-type-of error) (table-name-of error)))))

(defcondition* unconfirmed-destructive-drop-column-error (unconfirmed-destructive-alter-table-error)
  ()
  (:report (lambda (error stream)
             (format stream "Dropping the column ~S from table ~S is a destructive transformation"
                     (column-name-of error) (table-name-of error)))))

(defun update-table (name columns)
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
            (unless (equal-type-p (type-of table-column) (rdbms-type-for (type-of column) *database*) *database*)
              (handler-case
                  (alter-column-type name column)
                (error (e)
                       (declare (ignore e))
                       (with-simple-restart
                           (continue "Alter the table and let the data go")
                         (error 'unconfirmed-destructive-alter-column-type-error :table-name name :column-name column-name
                                :new-type (cl-rdbms::type-of column)))
                       (drop-column name column-name)
                       (add-column name column))))
            ;; add missing columns not present in the table
            (add-column name column))))
    ;; drop extra columns that are present in the table
    (dolist (table-column table-columns)
      (let ((column-name (name-of table-column)))
	(unless (find column-name columns :key (compose #'string-downcase #'name-of) :test #'equalp)
          (with-simple-restart
              (continue "Alter the table and let the data go")
            (error 'unconfirmed-destructive-drop-column-error :table-name name :column-name column-name))
	  (drop-column name column-name))))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Create, drop sequence

(defun create-sequence (name)
  (execute-ddl (make-instance 'sql-create-sequence :name name)))

(defun drop-sequence (name)
  (execute-ddl (make-instance 'sql-drop-sequence :name name)))

(defun sequence-exists-p (name)
  (not (null (member (string-downcase name) (list-sequences) :test 'equalp))))

(defun list-sequences ()
  (database-list-sequences *database*))

(defgeneric database-list-sequences (database)
  (:documentation "Returns the list of sequence names present in the database."))

(defun sequence-next (name)
  (caar (execute (make-instance 'sql-select :columns (list (make-instance 'sql-sequence-nextval-column :name name))))))
