;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms)

;;; THE CONTENT OF THIS FILE IS COPIED OVER FROM SOME OTHER LIBRARIES TO DECREASE DEPENDENCIES

(defmacro prog1-bind (var ret &body body)
  `(let ((,var ,ret))
    ,@body
    ,var))

(defun concatenate-symbol (&rest args)
  "Args are processed as parts of the result symbol with an exception: when a package is encountered then it is stored as the target package at intern."
  (let* ((package nil)
         (symbol-name (string-upcase
                       (with-output-to-string (str)
                         (dolist (arg args)
                           (typecase arg
                             (string (write-string arg str))
                             (package (setf package arg))
                             (symbol (unless package
                                       (setf package (symbol-package arg)))
                                     (write-string (symbol-name arg) str))
                             (integer (write-string (princ-to-string arg) str))
                             (character (write-char arg) str)
                             (t (error "Cannot convert argument ~S to symbol" arg))))))))
    (if package
        (intern symbol-name package)
        (intern symbol-name))))

(def macro eval-always (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@body))

(defmacro rebind (bindings &body body)
  `(let ,(loop
            :for symbol-name :in bindings
            :collect (list symbol-name symbol-name))
     ,@body))

(def (function eo) concatenate-string (&rest args)
  ;; don't inline, otherwise the compiler macro is kicked
  (apply #'concatenate 'string args))

(def compiler-macro concatenate-string (&rest args)
  `(concatenate 'string ,@args))

;; TODO these should probably hide their cl counterparts, because then inlined they reduce
;; to a mere CL:FIRST call if type information is available
(def (function io) first* (sequence)
  (elt sequence 0))

(def (function io) second* (sequence)
  (elt sequence 1))

(def (function io) third* (sequence)
  (elt sequence 2))

(def (function io) last* (sequence)
  (last-elt sequence))
