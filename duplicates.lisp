;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms)

;;; THE CONTENT OF THIS FILE IS COPIED OVER FROM SOME OTHER LIBRARIES TO DECREASE DEPENDENCIES

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

(defun length=1 (list)
  (and list (null (cdr list))))

(declaim (inline first* second* third*))

(defun first* (sequence)
  (elt sequence 0))

(defun second* (sequence)
  (elt sequence 1))

(defun third* (sequence)
  (elt sequence 2))

;;;;;;;;;;;;;;;;;;;
;;; from alexandria

(defmacro collecting (bindings &body body)
  "Similar to collecting in loop, iter and the less efficient but widely used push/nreverse method.
  Example:
    (collecting (foo bar)
      (collect 11 :into foo)
      (collect 22 :into bar)
      (collect 33 :into foo)
      (collect 44 :into bar)
      bar)
    =>
    (22 44)

    (collecting ()
       (collect 11)
       (collect 22)
       (collect 33))
    =>
    ; No value"
  `(%collecting () ,bindings ,@body))

(defmacro collecting* (bindings &body body)
  "Just like collecting, but returns (values ,@collected-lists) when exiting normally.
  Example:
    (collecting* (foo bar)
      (collect 11 :into foo)
      (collect 22 :into bar)
      (collect 33 :into foo)
      (collect 44 :into bar)
      bar) ; note the ignored return value
    =>
    (11 33)
    (22 44)

    (collecting* ()
       (collect 11)
       (collect 22)
       (collect 33))
    =>
    (11 22 33)"
  `(%collecting (:implicit-return t) ,bindings ,@body))

(defmacro %collecting ((&key implicit-return) bindings &body body)
  (unless bindings
    (setf bindings (list (gensym))))
  (let* ((names (mapcar (lambda (binding)
                          (if (consp binding)
                              (first binding)
                              binding))
                        bindings))
         (tails (mapcar (lambda (name)
                          (gensym (concatenate 'string (string name) "-TAIL")))
                        names))
         (single-mode-p (= (length names) 1)))
    (multiple-value-bind (remaining-forms declarations)
        (parse-body body)
      `(macrolet ((collect (what &key (into ',(when single-mode-p (first names))))
                    (if into
                        (case into
                          ,@(loop for name :in names
                                  for tail :in tails
                                  collect `(',name `(progn
                                                     (if ,',tail
                                                         (setf (cdr ,',tail) (cons ,what nil)
                                                               ,',tail (cdr ,',tail))
                                                         (setf ,',tail (cons ,what nil)
                                                               ,',name ,',tail))
                                                     (values))))
                          (t (error "Collect form into unknown variable ~A" into)))
                        (error "You must specify where to collect into when using multiple places"))))
        (let (,@bindings
              ,@tails)
          ,declarations
          ,@remaining-forms
          ,@(when implicit-return
              (if single-mode-p
                  (list (first names))
                  `((values ,@names)))))))))

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
