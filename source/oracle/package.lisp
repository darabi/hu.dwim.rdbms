;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :common-lisp-user)

(defpackage :hu.dwim.rdbms.oracle
  (:use :hu.dwim.common-lisp
        :hu.dwim.def
        :hu.dwim.defclass-star
        :hu.dwim.logger
        :hu.dwim.rdbms
        :hu.dwim.syntax-sugar
        :hu.dwim.util
        :local-time)

  (:shadowing-import-from #:hu.dwim.rdbms
                          #:type #:type-of #:log)

  (:shadow #:null))

(in-package :hu.dwim.rdbms.oracle)

#+#.(cl:when (cl:find-package "SWANK") '(:and))
(register-readtable-for-swank
 '(:hu.dwim.rdbms.sqlite) 'hu.dwim.rdbms::setup-readtable)

;; import all the internal symbol of :hu.dwim.rdbms into :hu.dwim.rdbms.oracle
(do-symbols (symbol :hu.dwim.rdbms)
  (when (and (eq (symbol-package symbol) #.(find-package :hu.dwim.rdbms))
             (not (find-symbol (symbol-name symbol) #.(find-package :hu.dwim.rdbms.oracle))))
    (import symbol)))

