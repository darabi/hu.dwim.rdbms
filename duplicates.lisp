;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms)

;;; THE CONTENT OF THIS FILE IS COPIED OVER FROM SOME OTHER LIBRARIES TO DECREASE DEPENDENCIES

;; dwim util
#+#.(cl:when (cl:find-package "SWANK") '(:and))
(defun setup-swank-readtable-alist (&rest package-name/readtable-setup-function-pairs)
  (loop for (package-names setup-function) :on package-name/readtable-setup-function-pairs :by #'cddr do
        (bind ((*readtable* (copy-readtable)))
          (funcall setup-function)
          (dolist (package-name (ensure-list package-names))
            (setf package-name (string package-name))
            (let ((entry (find package-name swank:*readtable-alist* :test #'string= :key #'car)))
              (unless entry
                (setf entry (cons package-name nil))
                (push entry swank:*readtable-alist*))
              (setf (cdr entry) *readtable*))))))

(defmacro enable-sharp-boolean-syntax ()
  "Copies *readtable* and enables #t and #f readers for t and nil in the copy."
  '(eval-when (:compile-toplevel :execute)
    (setf *readtable* (copy-readtable *readtable*))
    (%enable-sharp-boolean-syntax)))

(defun %enable-sharp-boolean-syntax ()
  (set-dispatch-macro-character
   #\# #\t
   (lambda (s c n)
     (declare (ignore s c n))
     t))
  (set-dispatch-macro-character
   #\# #\f
   (lambda (s c n)
     (declare (ignore s c n))
     nil)))

(defmacro aprog1 (ret &body body)
  `(prog1-bind it ,ret ,@body))

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

(def (function io) length=1 (sequence)
  (if (listp sequence)
      (and sequence
           (null (rest sequence)))
      (= 1 (length sequence))))

;; TODO these should probably hide their cl counterparts, because then inlined they reduce
;; to a mere CL:FIRST call if type information is available
(def (function io) first* (sequence)
  (elt sequence 0))

(def (function io) second* (sequence)
  (elt sequence 1))

(def (function io) third* (sequence)
  (elt sequence 2))

(def (function io) last* (sequence)
  (if (listp sequence)
      (last1 sequence)
      (elt sequence (1- (length sequence)))))

;;;;;;;;;;;;;;;;;;;
;;; from alexandria

(defun parse-body (body &key documentation whole)
  "Parses BODY into (values remaining-forms declarations doc-string).
Documentation strings are recognized only if DOCUMENTATION is true.
Syntax errors in body are signalled and WHOLE is used in the signal
arguments when given."
  (let ((doc nil)
        (decls nil)
        (current nil))
    (tagbody
     :declarations
       (setf current (car body))
       (when (and documentation (stringp current) (cdr body))
         (if doc
             (error "Too many documentation strings in ~S." (or whole body))
             (setf doc (pop body)))
         (go :declarations))
       (when (and (listp current)
                  (eq 'declare (first current)))
         (push (pop body) decls)
         (go :declarations)))
    (values body (nreverse decls) doc)))

(define-condition simple-style-warning (style-warning simple-warning)
  ())

(defun simple-style-warning (message &rest args)
  (warn 'simple-style-warning :format-control message :format-arguments args))
