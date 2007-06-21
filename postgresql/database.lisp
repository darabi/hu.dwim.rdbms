;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms.postgresql)

#.(file-header)

(defclass* postgresql (database)
  ())

(defclass* postgresql-transaction ()
  ())

(defparameter *unique-counter* 0)

(defun generate-unique-postgresql-name (base)
  (strcat base (incf *unique-counter*)))

(defconstant +maximum-rdbms-name-length+ 63)

;; this name mapping is not injective, different lisp names _may_ be mapped to the same rdbms name
(defmethod calculate-rdbms-name ((db postgresql) thing name)
  "Cuts off the end of names that are too long and appends the SXHASH of the original name."
  (let ((name-as-string (strcat "_" (string-downcase name))))
    (iter (for char :in-sequence "*\\/-")
          (nsubstitute #\_ char name-as-string :test #'char=))
    (let ((name-as-bytes (string-to-octets name-as-string :utf-8)))
      (when (> (length name-as-bytes)
               +maximum-rdbms-name-length+)
        (iter (while (> (length name-as-bytes)
                        (- +maximum-rdbms-name-length+ 8)))
              (setf name-as-string (subseq name-as-string 0 (1- (length name-as-string))))
              (setf name-as-bytes (string-to-octets name-as-string :utf-8)))
        (setf name-as-string
              (strcat name-as-string (format nil "~8,'0X" (logand (sxhash name-as-string)
                                                                  #.(1- (expt 2 32)))))))
      name-as-string)))
