;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-user)

(defpackage :cl-rdbms.postgresql
  (:shadowing-import-from #:cl-rdbms
    #:type #:type-of #:log)

  (:use
   :common-lisp
   :iterate
   :alexandria
   :anaphora
   :cl-def
   :cl-yalog
   :metabang-bind
   :defclass-star
   :cl-rdbms-system
   :cl-rdbms
   )

  (:shadowing-import-from :cl-rdbms
   #:unable-to-obtain-lock-error)

  (:export
   ))

(in-package :cl-rdbms.postgresql)

;; import all the internal symbol of :cl-rdbms into :cl-rdbms.postgresql
(do-symbols (symbol :cl-rdbms)
  (when (and (eq (symbol-package symbol) #.(find-package :cl-rdbms))
             (not (find-symbol (symbol-name symbol) #.(find-package :cl-rdbms.postgresql))))
    (import symbol)))

;; a nasty KLUDGE, please, FIXME! as soon as local-time gets date and time support...
(in-package :local-time)

(export '(parse-datestring))

(defun parse-datestring (string)
  (let* ((*default-timezone* +utc-zone+)
         (date (parse-timestring string :offset 0)))
    (unless (and date
                 (zerop (sec-of date))
                 (zerop (nsec-of date)))
      (error "~S is not a valid date string" string))
    date))

