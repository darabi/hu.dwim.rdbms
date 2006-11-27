;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms)

#.(file-header)

;; nothing postgres specific for now
#+nil(defmethod format-sql-syntax-node ((type sql-boolean-type) (database postgresql))
  (format-string "BOOL"))

