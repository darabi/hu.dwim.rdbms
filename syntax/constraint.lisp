(in-package :cl-rdbms)

#.(file-header)

(defclass* sql-constraint (sql-syntax-node)
  ((name
    nil
    :type symbol))
  (:documentation "An SQL constraint."))

(defclass* sql-constraint-with-tablespace (sql-constraint)
  ((tablespace
    nil
    :type symbol)))

(defclass* sql-primary-key-constraint (sql-constraint-with-tablespace)
  ())

(defclass* sql-null-constraint (sql-constraint)
  ())

(defclass* sql-not-null-constraint (sql-constraint)
  ())

(defclass* sql-unique-constraint (sql-constraint-with-tablespace)
  ())

(defmethod format-sql-syntax-node :before ((constraint sql-constraint) database)
  (awhen (name-of constraint)
    (write-string " CONSTRAINT" *sql-stream*)
    (write-char #\Space *sql-stream*)
    (format-sql-syntax-node it database)))

(defmethod format-sql-syntax-node ((constraint sql-primary-key-constraint) database)
  (write-string " PRIMARY KEY" *sql-stream*))

(defmethod format-sql-syntax-node ((constraint sql-null-constraint) database)
  (write-string " NULL" *sql-stream*))

(defmethod format-sql-syntax-node ((constraint sql-not-null-constraint) database)
  (write-string " NOT NULL" *sql-stream*))

(defmethod format-sql-syntax-node ((constraint sql-unique-constraint) database)
  (write-string " UNIQUE" *sql-stream*))
