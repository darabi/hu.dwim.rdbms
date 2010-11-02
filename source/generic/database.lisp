;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.rdbms)

;;;;;;
;;; Database

(def (class* e) database ()
  ((connection-specification
    :documentation "Backend specific connection data, usually a plist of args passed to the connect function.")
   (default-result-type
    'vector
    :type (member vector list cursor))
   (transaction-class
    :type standard-class
    :documentation "Transactions will be instances of this class. This class is created according to the generic method transaction-mixin-class.")
   (encoding
    :utf-8
    :type (member :utf-8 :us-ascii))
   (ddl-query-cache
    nil
    :type (or hash-table null))))

(def (constant e) +database-command-line-options+
  '((("database-host" #\Space)
     :type string
     :initial-value "localhost"
     :documentation "The server host name where the database is listening.")
    (("database-port" #\Space)
     :type integer
     :documentation "The server port where the database is listening.")
    (("database-name" #\Space)
     :type string
     :documentation "The database name that will be connected.")
    (("database-user-name" #\Space)
     :type string
     :documentation "The user name that is used to connect to the database.")
    (("database-password" #\Space)
     :type string
     :documentation "The password that is used to connect to the database.")))

(def method shared-initialize :after ((database database) slot-names
                                     &key transaction-mixin generated-transaction-class-name &allow-other-keys)
  (let ((classes (mapcar #'find-class (transaction-mixin-class database))))
    (setf (transaction-class-of database)
          (make-instance 'standard-class
                         :name generated-transaction-class-name
                         :direct-superclasses (aif transaction-mixin
                                                   (cons (find-class it) classes)
                                                   classes)))))

(def (generic e) transaction-mixin-class (database)
  (:documentation "Collects the transaction mixin classes which will be inherited by the transaction class instantiated by with-transaction when using this database.")

  (:method-combination list))

(def (macro e) with-database (database &body forms)
  "Evaluates FORMS within the dynamic scope of DATABASE."
  `(let ((*database* ,database))
    ,@forms))

;;;;;;
;;; RDBMS names

(def generic calculate-rdbms-name (database thing name)
  (:documentation "May be specialized to take name length and character set limitations into account.")
  (:method ((database database) thing name)
           (string-downcase name)))

(def function rdbms-name-for (name &optional thing)
  (declare (cl:type (or null (member :table :view :index :column :sequence)) thing))
  (calculate-rdbms-name *database* thing name))

(def function calculate-rdbms-name-with-utf-8-length-limit (name limit &key prefix)
  "Cuts off the end of names that are too long and appends the hash of the original name."
  (assert (>= limit 8))
  (bind ((name-as-string (string+ prefix (string-downcase name))))
    (iter (for char :in-sequence "+*\\/-~%")
          (nsubstitute #\_ char name-as-string :test #'char=))
    (bind ((name-as-bytes (string-to-octets name-as-string :encoding :utf-8)))
      (when (> (length name-as-bytes)
               limit)
        (bind ((hash (ironclad:byte-array-to-hex-string
                      (ironclad:digest-sequence :crc32 name-as-bytes))))
          ;; drop chars one-by-one because we have no idea about their UTF-8 length
          (iter (while (> (length name-as-bytes)
                          (- limit 8)))
                (setf name-as-string (subseq name-as-string 0 (1- (length name-as-string))))
                (setf name-as-bytes (string-to-octets name-as-string :encoding :utf-8)))
          (setf name-as-string (string+ name-as-string hash))))
      name-as-string)))

(def (function e) enable-ddl-query-cache (database)
  (setf (ddl-query-cache-of database) (make-hash-table)))

(def (function e) disable-ddl-query-cache (database)
  (setf (ddl-query-cache-of database) nil))

(def (function e) clear-ddl-query-cache (database)
  (disable-ddl-query-cache database)
  (enable-ddl-query-cache database))
