(in-package :cl-rdbms)

#.(file-header)

(define-syntax-node sql-constraint (sql-syntax-node)
  ((name
    nil
    :type string))
  (:documentation "An SQL constraint."))

(define-syntax-node sql-constraint-with-tablespace (sql-constraint)
  ((tablespace
    nil
    :type string)))

(define-syntax-node sql-primary-key-constraint (sql-constraint-with-tablespace)
  ())

(define-syntax-node sql-null-constraint (sql-constraint)
  ())

(define-syntax-node sql-not-null-constraint (sql-constraint)
  ())

(define-syntax-node sql-unique-constraint (sql-constraint-with-tablespace)
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
