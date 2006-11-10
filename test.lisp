;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms-test)

(eval-always
  (import (let ((*package* (find-package :cl-rdbms)))
            (read-from-string "(enable-sharp-boolean-syntax
                                log.dribble log.debug log.info log.warn log.error)"))))

(enable-sharp-boolean-syntax)

(def-suite :cl-rdbms :description "cl-rdbms tests")

(in-suite :cl-rdbms)

#+nil(test cl-rdbms
  (finishes))
