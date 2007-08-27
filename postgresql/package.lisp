;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-user)

(defpackage :cl-rdbms.postgresql
  (:shadowing-import-from #:cl-rdbms
    #:type #:type-of #:log)

  ;; TODO drop :arnesi, use :alexandria
  (:use :cl :iterate :arnesi :cl-def :metabang-bind :defclass-star :cl-rdbms-system :cl-rdbms)

  (:export
   ))

(in-package :cl-rdbms.postgresql)

;; import all the internal symbol of :cl-rdbms into :cl-rdbms.postgresql
(do-symbols (symbol :cl-rdbms)
  (when (and (eq (symbol-package symbol) #.(find-package :cl-rdbms))
             (not (find-symbol (symbol-name symbol) #.(find-package :cl-rdbms.postgresql))))
    (import symbol)))

