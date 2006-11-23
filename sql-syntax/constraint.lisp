(in-package :cl-rdbms)

#.(file-header)

(defclass* sql-constraint (sql-syntax-node)
  ()
  (:documentation "An SQL constraint."))

(defclass* sql-primary-key-constraint (sql-constraint)
  ())
