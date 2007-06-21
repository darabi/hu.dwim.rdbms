(in-package :cl-rdbms)

#.(file-header)

(define-syntax-node sql-constraint (named-sql-syntax-node)
  ()
  (:documentation "An SQL constraint."))

(define-syntax-node sql-constraint-with-tablespace (sql-constraint)
  ((tablespace
    nil
    :type string)))

(define-syntax-node sql-primary-key-constraint (sql-constraint-with-tablespace)
  ()
  (:format-sql-syntax-node
   (format-string " PRIMARY KEY")))

(define-syntax-node sql-null-constraint (sql-constraint)
  ()
  (:format-sql-syntax-node
   (format-string " NULL")))

(define-syntax-node sql-not-null-constraint (sql-constraint)
  ()
  (:format-sql-syntax-node
   (format-string " NOT NULL")))

(define-syntax-node sql-unique-constraint (sql-constraint-with-tablespace)
  ()
  (:format-sql-syntax-node
   (format-string " UNIQUE")))

(defmethod format-sql-syntax-node :before ((constraint sql-constraint) database)
  (awhen (name-of constraint)
    (format-string " CONSTRAINT")
    (format-char " ")
    (format-sql-syntax-node it database)))




