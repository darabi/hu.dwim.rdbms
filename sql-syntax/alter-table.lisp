;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms)

#.(file-header)

(defclass* sql-alter-table (sql-expression)
  ((table-name)
   (alter-table-action))
  (:documentation "An SQL ALTER TABLE statement."))

(defclass* sql-alter-table-action (sql-syntax-node)
  ())

(defclass* sql-alter-table-column-action (sql-alter-table-action)
  ((column-name)
   (column-type)))

(defclass* sql-alter-table-add-column-action (sql-alter-table-column-action)
  ((default-value)
   (constraints)))

(defclass* sql-alter-table-drop-column-action (sql-alter-table-column-action)
  ())

(defclass* sql-alter-table-alter-column-type-action (sql-alter-table-column-action)
  ((constraints)))

(defun database-column-type (column-type database)
  (database-get-type-specifier (first (listify column-type))
                               (rest (listify column-type))
                               database
                               (database-underlying-type database)))

(defmethod format-sql-syntax-node ((stmt sql-alter-table) database)
  (with-slots (table-name alter-table-action) stmt
    (write-string "ALTER TABLE " *sql-stream*)
    (format-sql-syntax-node table-name database)
    (write-char #\Space *sql-stream*)
    (format-sql-syntax-node alter-table-action database)))

;; TODO: output column constraints
(defmethod format-sql-syntax-node ((action sql-alter-table-add-column-action) database)
  (with-slots (column-name column-type) action
    (write-string "ADD " *sql-stream*)
    (format-sql-syntax-node column-name database)
    (write-char #\Space *sql-stream*)
    (format-sql-syntax-node column-type database)))

(defmethod format-sql-syntax-node ((action sql-alter-table-drop-column-action) database)
  (with-slots (column-name) action
    (write-string "DROP COLUMN " *sql-stream*)
    (format-sql-syntax-node column-name database)))

;; TODO: output column constraints
(defmethod format-sql-syntax-node ((action sql-alter-table-alter-column-type-action)
		       database)
  (with-slots (column-name column-type) action
    (write-string "ALTER COLUMN " *sql-stream*)
    (format-sql-syntax-node column-name database)
    (write-string " TYPE " *sql-stream*)
    (format-sql-syntax-node column-type database)))

(defun alter-table (table-name
		    alter-table-column-action
		    &key (database *default-database*))
  "Alter the given table by executing an action. It may add a new column,
drop or change the type of an existing one."
  (let* ((table-name (etypecase table-name 
                       (symbol (sql-expression :attribute table-name))
                       (string (sql-expression :attribute table-name))
                       (sql-ident table-name)))
         (stmt (make-instance 'sql-alter-table
                              :table-name table-name
                              :alter-table-action alter-table-column-action)))
    (execute-command stmt :database database)))

(defun alter-table-add-column (table-name
			       column-name
			       column-type
			       constraints
			       &key (database *default-database*))
  (alter-table table-name
               (make-instance 'sql-alter-table-add-column-action
                              :column-name column-name
                              :column-type column-type
                              :constraints constraints)
               :database database))

(defun alter-table-drop-column (table-name
				column-name
				&key (database *default-database*))
  (alter-table table-name
               (make-instance 'sql-alter-table-drop-column-action :column-name column-name)
               :database database))

(defun alter-table-alter-column-type (table-name
				      column-name
				      column-type
                                      constraints
				      &key (database *default-database*))
  (alter-table table-name
               (make-instance 'sql-alter-table-alter-column-type-action
                              :column-name column-name
                              :column-type column-type
                              :constraints constraints)
               :database database))

(defun update-table (table-name
		     columns
		     &key (database *default-database*))
  (if (not (table-exists-p table-name))
      (create-table table-name columns :database database)
      (update-existing-table table-name columns :database database)))

;; TODO: handle constraints
(defun update-existing-table (table-name
			      columns
			      &key (database *default-database*))
  (let ((table-columns (list-attribute-types table-name :database database)))
    ;; create new columns that are missing from the table
    (dolist (column columns)
      (let* ((column-name (first column))
	     (table-column (find (database-identifier column-name database) table-columns
				 :key #'first
				 :test #'string=)))
	(unless table-column
          (alter-table-add-column table-name
                                  (column-name-from-arg column-name)
                                  (second column)
                                  nil
                                  :database database))
	;; change column type where needed
        ;; TODO: handle constraints
        
	(when (and table-column
		   (not (string= (column-type-specifier-for-table-column table-column
									 :database database)
				 (column-type-specifier-for-column column
								   :database database))))
	  (handler-case
              (alter-table-alter-column-type table-name
                                             (column-name-from-arg column-name)
                                             (second column)
                                             nil
                                             :database database)
            (sql-database-data-error ()
              (warn "Cannot alter table column type, column will be dropped and recreated")
              (alter-table-drop-column table-name
                                       (column-name-from-arg column-name)
                                       :database database)
              (alter-table-add-column  table-name
                                       (column-name-from-arg column-name)
                                       (second column)
                                       nil
                                       :database database))))))
    ;; drop extra columns that are present in the table
    (dolist (table-column table-columns)
      (let ((column-name (first table-column)))
	(unless (find column-name columns 
		      :key (lambda (column) (database-identifier (car column) database))
		      :test #'string=)
	  (alter-table-drop-column table-name
				   (column-name-from-arg column-name)
				   :database database))))))

(defun column-type-specifier-for-table-column (table-column
					       &key (database *default-database*))
  "String representation for a table column's type. Parameters are enclosed within braces."
  ;; FIXME: this is postgresql specific (unfortunately we do not get back the type which was used to create the table)
  (let ((type-name (second table-column)))
    (if (eq type-name :bpchar)
        (setf type-name :char))
    (if (and (third table-column) (not (eq type-name :int4)) (not (eq type-name :bool)))
        (concatenate 'string
                     (symbol-name type-name)
                     "(" (write-to-string (third table-column)) ")")
        (concatenate 'string
                     (symbol-name type-name)))))

(defun column-type-specifier-for-column (column
					 &key (database *default-database*))
  "String representation for a column specification's type.
Parameters are enclosed within braces."
  (if (listp (second column))
      (database-get-type-specifier (first (second column))
				   (cdr (second column))
				   database
				   (database-type database))
      (database-get-type-specifier (second column)
				   nil
				   database
				   (database-type database))))
