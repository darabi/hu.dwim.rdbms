;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms)

(def constant +default-sql-syntax-open-character+ #\[)

(def constant +default-sql-syntax-close-character+ #\])

;; #\, conflicts with backquote and comma, but it's ok, because it's a reader, so it couln't work
;; transparently inside `(foo [select ,bar]) anyway, so we use #\, for sql-unquote. see the SQL macro
;; if you want to use it inside a backquote.
(def constant +default-sql-syntax-unquote-character+ #\,)

(def function make-sql-reader (end-char unquote-char)
  (labels ((sql-unquote-reader (stream char)
             (declare (ignore char))
             (list 'sql-unquote (read stream t nil t)))
           (sql-reader (stream char)
             (declare (ignore char))
             (bind ((*readtable* (copy-readtable)))
               (set-macro-character unquote-char #'sql-unquote-reader)
               (bind ((body (read-delimited-list end-char stream t)))
                 ;; TODO these cause some warnings, but it's a headache to load stuff in such an order that...
                 (expand-sql-ast-into-lambda-form
                  (compile-sexp-sql body))))))
    #'sql-reader))

(def (macro e) enable-sql-syntax (&optional (open-char +default-sql-syntax-open-character+)
                                            (close-char +default-sql-syntax-close-character+)
                                            (unquote-char +default-sql-syntax-unquote-character+))
  "Enable [select ,foo ,bar] reader for the rest of the file (being loaded or compiled).
Be careful when using in different situations, because it modifies *readtable*."
  `(eval-when (:compile-toplevel :execute)
    ;; this COPY-READTABLE is important because unaware users may alter readtables they didn't intend to.
    ;; but because of this it does not work from the Slime repl.
    (setf *readtable* (copy-readtable))
    (set-macro-character ,open-char (make-sql-reader ,close-char ,unquote-char) t *readtable*)
    (set-syntax-from-char ,close-char #\) *readtable*)))
